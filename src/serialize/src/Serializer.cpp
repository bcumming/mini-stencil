// Created by Andrea Arteaga, MeteoSwiss
// Email: andyspiros@gmail.com
// January 2013

#include "SerializationFramework.h"
#include "libjson.h"
#include <algorithm>
#include <fstream>
#include <sstream>



void Serializer::AddGlobalMetaInfo(const MetaInfo& info)
{
    globalMetainfo_.push_back(info);
}


void Serializer::AddFieldMetaInfo(std::string fieldName, const MetaInfo& info)
{
    if (fieldInfos_.count(fieldName) == 0)
    {
        std::string message = "Error while adding field metainfo: field '";
        message += fieldName;
        message += "' has not been found.";
        throw FieldNotFound(message);
    }

    fieldInfos_[fieldName].AddMetaInfo(info);
}

void Serializer::ImportField(std::string fieldName)
{
    // Check whether the field is already imported
    if (fieldInfos_.count(fieldName))
        return;

    // Otherwise, deserialize the metainfo from file
    std::string fnameMeta = fileBaseName(fieldName) + ".json";

    // Prepare exception for failure case
    std::string errorMessage = "Field '";
    errorMessage += fieldName + "' not found";
    FieldNotFound error(errorMessage);

    // Try to open file
    std::ifstream fs;
    try
    {
        fs.open(fnameMeta.c_str());
    }
    catch (...) {
        throw error;
    }
    if (!fs.good())
        throw error;

    // Get file dimension
    fs.seekg(0, std::ios::end);
    std::streamoff dimension = fs.tellg();
    fs.seekg(0, std::ios::beg);

    // Check that the dimension is valid
    if (dimension < 0)
        throw error;

    // Read in string
    std::string fContent;
    fContent.resize(static_cast<size_t>(dimension));
    std::copy(std::istreambuf_iterator<char>(fs),
              std::istreambuf_iterator<char>(),
              fContent.begin()
          );

    // Try to parse JSON
    JSONNode mainNode;
    try
    {
        mainNode = libjson::parse(fContent);
    }
    catch(...)
    {
        throw error;
    }

    DataFieldInfo fakeInfo;
    MetaDataSerializer::Deserialize(*this, mainNode, fakeInfo);
}

void Serializer::LoadMetadata(std::string fieldName)
{
//    assert(Serializer::singleFile_);
    std::string fname = fileBaseName(fieldName) + ".json";

    // Try to open file or exit without exception
    std::fstream fs;
    try
    {
        fs.open(fname.c_str(), std::ios_base::in);
    }
    catch(...)
    {
        return;
    }
    if (!fs.good())
        return;

    // Get file dimension
    fs.seekg(0, std::ios::end);
    std::streamoff dimension = fs.tellg();
    fs.seekg(0, std::ios::beg);

    // Check that the dimension is valid
    if (dimension < 0)
        throw SerializationException("Failed to load metadata");

    // Read contents
    std::string fContent;
    fContent.resize(static_cast<size_t>(dimension));
    std::copy(std::istreambuf_iterator<char>(fs),
              std::istreambuf_iterator<char>(),
              fContent.begin()
    );

    // Parse metadata
    MetaDataSerializer::Deserialize(*this, libjson::parse(fContent));
}


//////////////////////// INITIALIZATION OF INPUT/OUTPUT ////////////////////////


size_t Serializer::InitializeOutput(const DataFieldInfo& fieldInfo,
                                 const SavePoint& savePoint)
{
    if (!readwrite_)
    {
        std::string message = "Serializer opened in read-only mode. Save is not"
                              "allowed.";
        throw SerializationException(message);
    }

    // Get file names
    std::string fnameMeta = fileBaseName(fieldInfo.name()) + ".json";
    std::string fnameData = fileBaseName(fieldInfo.name()) + ".dat";

    ptrdiff_t offset = -1;
    bool newField = (fieldInfos_.count(fieldInfo.name()) == 0);

    // Truncate files if overwrite flag is set and this is the first operation
    if (!singleFile_ && readwrite_ && newField)
    {
        std::fstream fsMeta(fnameMeta.c_str(),
                            std::fstream::out | std::fstream::trunc);
        std::fstream fsData(fnameData.c_str(),
                            std::fstream::out | std::fstream::trunc);
    }

    // Add the field to the list if not already present
    if (newField)
    {
        fieldInfos_[fieldInfo.name()] = fieldInfo;
    }

    // Get offset
    try
    {
        // Open at the end and check that the stream is valid
        std::fstream fsDatRead(fnameData.c_str(),
                               std::ios_base::in | std::ios_base::binary);
        if (!fsDatRead.good())
            throw std::exception();

        fsDatRead.seekg(0, std::ios_base::end);
        if ((offset = fsDatRead.tellg()) < 0)
            throw std::exception();

        // Close
        fsDatRead.close();
    }
    catch(...)
    {
        // File does not exist
        offset = 0;
    }

    // Check whether savepoint is valid and add if needed
    if (indexTable_.size() == 0 || indexTable_.back().savePoint() != savePoint)
    {
        // Check that no other "old" savepoint is equal to the current one
        for (IndexTable::const_iterator iter = indexTable_.begin();
                                        iter != indexTable_.end(); ++iter)
        {
            if (iter->second.savePoint() == savePoint)
            {
                std::string message = "It is not allowed to save the field '";
                message += fieldInfo.name() + "' at the preceding savepoint ";
                message += savePoint.ToString();
                throw SerializationException(message);
            }
        }

        // Add the new savepoint entry in the index table
        IndexTableEntry newEntry;
        newEntry.set_SavePoint(savePoint);
        indexTable_.push_back(newEntry);
    }

    // Add the field to the index table entry
    indexTable_.back().AddField(fieldInfo.name(), static_cast<size_t>(offset));

    // Store savepoint as last used savepoint
    lastSavePoint_ = savePoint;

    // Serialize metadata
    const std::string fContent =
        (singleFile_? MetaDataSerializer::Serialize(*this) :
                      MetaDataSerializer::Serialize(*this, fieldInfo))
        .write_formatted();

    // Store serialized metadata
    std::fstream fsJson(fnameMeta.c_str(),
            std::ios_base::out | std::ios_base::trunc);
    fsJson << fContent;
    fsJson.close();

    return static_cast<size_t>(offset);
}

//TODO merge with ImportField
void Serializer::InitializeInput(std::string fieldName)
{
    bool newField = (fieldInfos_.count(fieldName) == 0);

    // Prepare exception for failure case
    std::string errorMessage = "Field '";
    errorMessage += fieldName + "' not found ";
    FieldNotFound error(errorMessage);

    // Get file names
    std::string fnameMeta = fileBaseName(fieldName) + ".json";
    std::string fnameData = fileBaseName(fieldName) + ".dat";

    if (newField)
    {
        // If in read-write mode, delete files and throw exception
        if (readwrite_)
        {
            std::fstream fsMeta(fnameMeta.c_str(),
                                std::fstream::out | std::fstream::trunc);
            std::fstream fsData(fnameData.c_str(),
                                std::fstream::out | std::fstream::trunc);

            std::string message = "Field ";
            message += fieldName + " can not be loaded as it has not been yet "
                                   "stored";
            throw SerializationException(message);
        }

        // Try to open file
        std::ifstream fs;
        try
        {
            fs.open(fnameMeta.c_str());
        }
        catch (...) {
            throw error;
        }
        if (!fs.good())
            throw error;

        // Get file dimension
        fs.seekg(0, std::ios::end);
        std::streamoff dimension = fs.tellg();
        fs.seekg(0, std::ios::beg);

        // Check that the dimension is valid
        if (dimension < 0)
            throw error;

        // Read in string
        std::string fContent;
        fContent.resize(static_cast<size_t>(dimension));
        std::copy(std::istreambuf_iterator<char>(fs),
                  std::istreambuf_iterator<char>(),
                  fContent.begin()
              );

        // Parse JSON
        JSONNode node = libjson::parse(fContent);
        node.preparse();

        // Deserialize JSON
        if (singleFile_)
            MetaDataSerializer::Deserialize(*this, node);
        else
            MetaDataSerializer::Deserialize(*this, node, DataFieldInfo());

        // If field name still not present, throw exception
        if (fieldInfos_.count(fieldName) == 0)
            throw error;
    }
}

size_t Serializer::GetInputSavePointOffset(std::string fieldName,
                                const SavePoint& savePoint, int instance)
{
    InitializeInput(fieldName);

    // Get offset and check that it is valid
    return indexTable_.LookupFieldOffset(fieldName, savePoint, instance);
}

const DataFieldInfo& Serializer::GetFieldInfo(std::string fieldName)
{
    // Try to import field info
    ImportField(fieldName);
    if (fieldInfos_.count(fieldName))
        return fieldInfos_[fieldName];

    // The field is not found
    std::string message = "Data field '";
    message.append(fieldName);
    message.append("' not found in serializer");
    throw FieldNotFound(message);
}

std::vector<const SavePoint*> Serializer::FindSavePointByName(std::string savePointName)
{
    return FindSavePointByName(savePointName, savePointName);
}

std::vector<const SavePoint*> Serializer::FindSavePointByName(std::string fieldName, std::string savePointName)
{
    InitializeInput(fieldName);
    std::vector<const SavePoint*> matchSavePoints;
    for (IndexTable::iterator iter = indexTable_.begin(); iter != indexTable_.end(); ++iter)
    {
        const SavePoint& savePoint = iter->second.savePoint();
        if (savePoint.name() == savePointName)
        {
            matchSavePoints.push_back(&savePoint);
        }
    }
    return matchSavePoints;
}

