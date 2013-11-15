// Created by Andrea Arteaga, MeteoSwiss
// Email: andyspiros@gmail.com
// January 2013

#include "SerializationFramework.h"
#include "libjson.h"
#include <cassert>





//////////////////////////////// SERIALIZATION ////////////////////////////////

JSONNode MetaDataSerializer::Serialize(const Serializer& serializer)
{
    assert(serializer.singleFile_);

    // GlobalInfo
    JSONNode globalN = SerializeGlobalInfo(serializer);

    // Data fields info
    JSONNode fieldsN;
    fieldsN.set_name("DataFieldsTable");
    fieldsN.cast(JSON_ARRAY);

    int i = 0;
    for(std::map<std::string, DataFieldInfo>::const_iterator
            iter = serializer.fieldInfos_.begin();
            iter != serializer.fieldInfos_.end();
            ++iter, ++i)
    {
        // Wrapper around DataFieldInfo node
        JSONNode fieldWrapperN;
        fieldWrapperN.push_back(iter->second.Serialize(i));
        fieldsN.push_back(fieldWrapperN);
    }


    // IndexTable
    JSONNode indexN = serializer.indexTable().Serialize();

    // Put things together
    JSONNode mainN;
    mainN.push_back(globalN);
    mainN.push_back(fieldsN);
    mainN.push_back(indexN);

    return mainN;
}


JSONNode MetaDataSerializer::Serialize(const Serializer& serializer, const DataFieldInfo& info)
{
    assert(!serializer.singleFile_);

    // GlobalInfo
    JSONNode globalN = SerializeGlobalInfo(serializer);

    // DataFieldInfo
    JSONNode fieldN = info.Serialize();

    // IndexTable
    JSONNode indexN = serializer.indexTable().Serialize(info.name());

    // Put things together
    JSONNode mainN;
    mainN.push_back(globalN);
    mainN.push_back(fieldN);
    mainN.push_back(indexN);

    return mainN;
}


JSONNode MetaDataSerializer::SerializeGlobalInfo(const Serializer& serializer)
{
    JSONNode globalN;

    std::ostringstream versionStream;
    versionStream << serializer.versionMajor_
                  << "." << serializer.versionMinor_;

    std::string format;
    if (serializer.singleFile_) format = "single-file";
    else format = "many-files";

    // Basic information
    globalN.set_name("GlobalInfo");
    globalN.push_back(JSONNode("Version", versionStream.str()));
    globalN.push_back(JSONNode("FormatType", format));
    globalN.push_back(JSONNode("Author", serializer.author_));

    // serializing global metadata
    for (std::vector<MetaInfo>::const_iterator
            iter = serializer.globalMetainfo_.begin();
            iter != serializer.globalMetainfo_.end();
            ++iter)
    {
        globalN.push_back(iter->Serialize());
    }

    return globalN;
}




/////////////////////////////// DESERIALIZATION ///////////////////////////////


void MetaDataSerializer::Deserialize(Serializer& serializer, const JSONNode& node)
{
    assert(serializer.singleFile_);

    serializer.globalMetainfo_.clear();
    serializer.fieldInfos_.clear();
    serializer.indexTable_.clear();

    // GlobalInfo
    try
    {
        DeserializeGlobalInfo(serializer, node.at("GlobalInfo"));
    }
    catch(std::exception& e)
    {
        throw ParseError("Malformed JSON: bad GlobalInfo block");
    }

    // DatafieldsTable
    try
    {
        const JSONNode fieldsTable = node.at("DataFieldsTable");
        for (JSONNode::const_iterator iter = fieldsTable.begin();
                                      iter != fieldsTable.end(); ++iter)
        {
            DataFieldInfo fieldInfo;
            fieldInfo.Deserialize((*iter).at(0));
            serializer.AddFieldInfo(fieldInfo);
        }
    }
    catch(std::exception& e)
    {
        throw ParseError("Malformed JSON: bad DataFieldsTable block");
    }

    // IndexTable
    try
    {
        serializer.indexTable_.Deserialize(node.at("SavePoints"));
    }
    catch(std::exception& e)
    {
        throw ParseError("Malformed JSON: bad SavePoints block");
    }

}

void MetaDataSerializer::Deserialize(Serializer& serializer, const JSONNode& node,
                             const DataFieldInfo&)
{
    assert(!serializer.singleFile_);

    // TODO: Maybe skip this
    try
    {
        DeserializeGlobalInfo(serializer, node.at("GlobalInfo"));
    }
    catch(std::exception& e)
    {
        throw ParseError("Malformed JSON: bad GlobalInfo block");
    }

    try
    {
        DataFieldInfo fieldInfo;
        fieldInfo.Deserialize(node.at("DataFieldInfo"));
        serializer.AddFieldInfo(fieldInfo);
    }
    catch(std::exception& e)
    {
        throw ParseError("Malformed JSON: bad DataFieldInfo block");
    }

    try
    {
        serializer.indexTable_.Deserialize(node.at("SavePoints"));
    }
    catch(std::exception& e)
    {
        throw ParseError("Malformed JSON: bad SavePoints block");
    }
}



void MetaDataSerializer::DeserializeGlobalInfo(Serializer& serializer, const JSONNode& node)
{
    // Retrieve version
    try
    {
        JSONNode versionN = node.at("Version");

        std::string versionStr = versionN.as_string();
        size_t dotPos = versionStr.find('.');
        versionStr[dotPos] = ' ';

        int versionMajor, versionMinor;
        std::stringstream versionStream;
        versionStream << versionStr;
        versionStream >> versionMajor >> versionMinor;

        serializer.set_Version(versionMajor, versionMinor);
    }
    catch(std::exception&)
    {
        throw BadGlobalInfo("Bad version in GlobalInfo");
    }


    // Retrieve format type (just checking whether it matches)
    try
    {
        JSONNode formatN = node.at("FormatType");
        std::string formatStr = formatN.as_string();
        bool wantsSingleFile = (formatStr == "single-file");

        if (!wantsSingleFile && formatStr != "many-files")
            throw formatStr;

        if (serializer.singleFile_ != wantsSingleFile)
            throw wantsSingleFile;
    }
    catch (std::string& formatError)
    {
        std::string message ("FormatType '");
        message.append(formatError);
        message.append("' not recognized");
        throw BadGlobalInfo(message);
    }
    catch(bool& e)
    {
        std::string message = "Data stored as ";
        if (e)
            message.append("single-file, but Serializer is in many-files");
        else
            message.append("many-files, but Serializer is in single-file");
        message.append(" mode");
        throw BadGlobalInfo(message);
    }
    catch (std::exception&)
    {
        throw BadGlobalInfo("Bad format in GlobalInfo");
    }


    // Retrieve author
    try
    {
        serializer.set_Author(node.at("Author").as_string());
    }
    catch (std::exception&)
    {
        throw BadGlobalInfo("Bad author in GlobalInfo");
    }


    // TODO: deserialize global metadata
}
