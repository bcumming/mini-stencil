// Created by Andrea Arteaga, MeteoSwiss
// Email: andyspiros@gmail.com
// March 2013

#pragma once

#include "GCL.h"
//TODO we should remove the next include once the forward declaration with template is solved
#include "proc_grids_2D.h"
#include <vector>
#include <map>
#include <string>

// Forward declaration
//TODO forward declaration fails with template parameters
//namespace GCL
//{
//    template<typename CYCLIC>
//    class MPI_2D_Process_grid_t;
//}

class DistributedSerializer
{
    typedef GCL::MPI_2D_process_grid_t<GCL::gcl_utils::boollist<2> > GridType;

public:
    // TODO: constructor
    // TODO: destructor frees the grid

    void Init(MPI_Comm communicator);
    void Init(MPI_Comm communicator, Serializer& serializer);

    void Save(const IJKRealField& field, const SavePoint& savePoint, std::string fieldName);
    void Load(IJKRealField& field, const SavePoint& savePoint, std::string fieldName);

private:
    std::vector<int> gatherSizes(const IJKSize& size, const IJKBoundary& globalBoundary);

    GridType* pGrid_;
    std::map<std::string, std::vector<int> > subDomains_;
    Serializer* pSerializer_;
};
