// Created by Andrea Arteaga, MeteoSwiss
// Email: andyspiros@gmail.com
// January-February 2013

#include "SerializationFramework.h"
#include "libjson.h"

JSONNode IndexTableEntry::Serialize(int id, std::string fieldName) const
{
    bool singleFile = (fieldName == "");

    JSONNode entryN;
    entryN.set_name("SavePointInfo");

    const std::vector<MetaInfo>& metainfo = savePoint_.metaInfo();

    entryN.push_back(JSONNode("Name", savePoint_.name()));
    entryN.push_back(JSONNode("Id", id));

    for(std::vector<MetaInfo>::const_iterator m = metainfo.begin();
                                              m != metainfo.end(); ++m)
    {
        entryN.push_back(m->Serialize());
    }

    // Add names of fields saved at savepoint
    JSONNode savedFieldsN;
    savedFieldsN.cast(JSON_ARRAY);
    savedFieldsN.set_name("SavedFields");

    for(size_t f = 0; f < fields_.size(); ++f)
    {
        if (!singleFile && fields_[f] != fieldName)
            continue;

        JSONNode savedFieldWrapper;
        savedFieldWrapper.push_back(JSONNode("Field", fields_[f]));
        savedFieldWrapper.push_back(JSONNode("Offset", offsets_[f]));

        savedFieldsN.push_back(savedFieldWrapper);
    }

    entryN.push_back(savedFieldsN);

    return entryN;
}


int IndexTableEntry::Deserialize(const JSONNode& node)
{
    savePoint_.clear();
    fields_.clear();
    offsets_.clear();

    int savePointID = -1;

    // Get name, id and other meta information
    for (JSONNode::const_iterator dataIter = node.begin();
                                  dataIter != node.end(); ++dataIter)
    {
        const std::string dataName = (*dataIter).name();

        if (dataName == "Name")
        {
            savePoint_.set_Name((*dataIter).as_string());
        }
        else if (dataName == "Id")
        {
            savePointID = static_cast<int>((*dataIter).as_int());
        }
        else if (dataName == "SavedFields")
        {
            // Will be interpreted later
            continue;
        }
        else
        {
            MetaInfo metainfo;
            metainfo.Deserialize(*dataIter);
            savePoint_.AddMetaInfo(metainfo);
        }
    }

    // Get saved fields
    const JSONNode savedFieldsN = node.at("SavedFields");

    for (JSONNode::const_iterator savfield = savedFieldsN.begin();
            savfield != savedFieldsN.end(); ++savfield)
    {
        std::string fieldName = (*savfield).at("Field").as_string();
        size_t offset = static_cast<size_t>((*savfield).at("Offset").as_int());

        AddField(fieldName, offset);
    }

    return savePointID;
}


