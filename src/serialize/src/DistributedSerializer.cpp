// Created by Andrea Arteaga, MeteoSwiss
// Email: andyspiros@gmail.com
// March 2013

#include "GCL.h"
#include "SharedInfrastructure.h"
#include "CommunicationFramework.h"
#include "SerializationFramework.h"

void DistributedSerializer::Init(MPI_Comm communicator)
{
    pGrid_ = new GridType(GCL::gcl_utils::boollist<2>(true, true), communicator);
    subDomains_.clear();
}

void DistributedSerializer::Init(MPI_Comm communicator, Serializer& serializer)
{
    Init(communicator);

    // The serializer is useful only for root
    if (pGrid_->proc(0, 0) == 0)
        pSerializer_ = &serializer;
    else
        pSerializer_ = 0;
}


void DistributedSerializer::Save(const IJKRealField& field, const SavePoint& savePoint, std::string fieldName)
{
    if (subDomains_.count(fieldName) == 0)
        subDomains_[fieldName] = gatherSizes(field.calculationDomain(), field.boundary());

    const int commSize = pGrid_->size();
    const std::vector<int>& subDomains = subDomains_.at(fieldName);

    // Initialize global field
    IJKSize globalDomain;
    globalDomain.Init(subDomains[commSize*3-1]+1 - subDomains[0], subDomains[commSize*4-1]+1 - subDomains[commSize], field.calculationDomain().kSize());
    KBoundary globalKBoundary;
    globalKBoundary.Init(field.boundary().kMinusOffset(), field.boundary().kPlusOffset());
    IJKRealField globalField;
    if (pGrid_->proc(0, 0) == 0)
        globalField.Init(fieldName, globalDomain, globalKBoundary);

    // Remove const qualifier from field. Notice that the allToAll job has
    // not compile-time const-correctness because this depends on the dynamic
    // scatterOrGather value. Anyway, it is ensured that the local field is not
    // modified in a gather operation (as weel as the global field is not
    // modified in a scatter operation).
    IJKRealField& nonConstField = const_cast<IJKRealField&>(field);

    // Perform gather
    RealAllToAll allToAll;
    allToAll.Init("DistributedSerializer", pGrid_->communicator, subDomains_[fieldName]);
    allToAll.AddJob(nonConstField, globalField, cGather);
    allToAll.Start();
    allToAll.Wait();

    // Serialize on root
    if(pGrid_->proc(0, 0) == 0)
    {
        pSerializer_->Save(globalField, savePoint);
    }
}


void DistributedSerializer::Load(IJKRealField& field, const SavePoint& savePoint, std::string fieldName)
{
    if (subDomains_.count(fieldName) == 0)
        subDomains_[fieldName] = gatherSizes(field.calculationDomain(), field.boundary());

    const int commSize = pGrid_->size();
    const std::vector<int>& subDomains = subDomains_.at(fieldName);

    // Initialize global field
    IJKSize globalDomain;
    globalDomain.Init(subDomains[commSize*3-1]+1 - subDomains[0], subDomains[commSize*4-1]+1 - subDomains[commSize], field.calculationDomain().kSize());
    KBoundary globalKBoundary;
    globalKBoundary.Init(field.boundary().kMinusOffset(), field.boundary().kPlusOffset());
    IJKRealField globalField;
    if (pGrid_->proc(0, 0) == 0)
        globalField.Init(fieldName, globalDomain, globalKBoundary);

    // Deserialize on root
    if(pGrid_->proc(0, 0) == 0)
    {
        pSerializer_->Load(globalField, savePoint);
    }

    // Perform scatter
    RealAllToAll allToAll;
    allToAll.Init("DistributedSerializer", pGrid_->communicator, subDomains_[fieldName]);
    allToAll.AddJob(field, globalField, cScatter);
    allToAll.Start();
    allToAll.Wait();
}


std::vector<int> DistributedSerializer::gatherSizes(const IJKSize& size, const IJKBoundary& globalBoundary)
{
    // Initialize grid (make it periodic just because GCL works better in
    // periodic mode though it is ininfluent for our task).
    const int commSize = pGrid_->size();
    const bool isRoot = pGrid_->proc(0, 0) == 0;

    // Prepare buffers
    std::vector<int> recvBuffer(isRoot ? (2*commSize) : 0);
    int sendBuffer[2] = { size.iSize() , size.jSize() };

    // Gather sizes
    MPI_Gather(sendBuffer, 2, MPI_INT, &recvBuffer[0], 2, MPI_INT, 0, pGrid_->communicator);

    // Reconstruct subdomains array
    std::vector<int> subDomains(4*commSize);
    if (isRoot)
    {
        int iStart = -globalBoundary.iMinusOffset() + 1, jStart, iEnd, jEnd;

        for (int pI = 0; pI < pGrid_->R; ++pI)
        {
            jStart = -globalBoundary.jMinusOffset() + 1;
            for (int pJ = 0; pJ < pGrid_->C; ++pJ)
            {
                GCL::array<int, 2> coords; coords[0] = pI; coords[1] = pJ;
                int rrank = pGrid_->abs_proc(coords);

                iEnd = iStart + recvBuffer[2*rrank+0] - 1;
                jEnd = jStart + recvBuffer[2*rrank+1] - 1;

                subDomains[rrank + 0*commSize] = iStart;
                subDomains[rrank + 1*commSize] = jStart;
                subDomains[rrank + 2*commSize] = iEnd;
                subDomains[rrank + 3*commSize] = jEnd;

                jStart = jEnd + 1;
            }
            iStart = iEnd + 1;
        }
    }

    // Broadcast array to all other processes
    MPI_Bcast(&subDomains[0], 4*commSize, MPI_INT, 0, pGrid_->communicator);

    return subDomains;
}

