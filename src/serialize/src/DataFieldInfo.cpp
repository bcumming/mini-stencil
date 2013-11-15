// Created by Andrea Arteaga, MeteoSwiss
// Email: andyspiros@gmail.com
// January-February 2013

#include "SerializationFramework.h"
#include "libjson.h"

JSONNode DataFieldInfo::Serialize(int id) const
{
    JSONNode fieldNode;
    fieldNode.set_name("DataFieldInfo");

    // Basic information
    fieldNode.push_back(JSONNode("Name", name_));
    if (id >= 0)
        fieldNode.push_back(JSONNode("Id", id));

    fieldNode.push_back(JSONNode("ElementType", type_));
    fieldNode.push_back(JSONNode("BytesPerElement", bytesPerElement_));
    fieldNode.push_back(JSONNode("Rank", rank_));

    // Size
    fieldNode.push_back(JSONNode("ISize", iSize_));
    fieldNode.push_back(JSONNode("JSize", jSize_));
    fieldNode.push_back(JSONNode("KSize", kSize_));
    if (rank_ >= 4) fieldNode.push_back(JSONNode("LSize", lSize_));

    // Halo
    fieldNode.push_back(JSONNode("IMinusHaloSize", iMinusHalo_));
    fieldNode.push_back(JSONNode("IPlusHaloSize", iPlusHalo_));
    fieldNode.push_back(JSONNode("JMinusHaloSize", jMinusHalo_));
    fieldNode.push_back(JSONNode("JPlusHaloSize", jPlusHalo_));
    fieldNode.push_back(JSONNode("KMinusHaloSize", kMinusHalo_));
    fieldNode.push_back(JSONNode("KPlusHaloSize", kPlusHalo_));
    if (rank_ >= 4)
    {
        fieldNode.push_back(JSONNode("LMinusHaloSize", lMinusHalo_));
        fieldNode.push_back(JSONNode("LPlusHaloSize", lPlusHalo_));
    }

    // Serialize the metadata associated with the field
    for (std::vector<MetaInfo>::const_iterator iter = metainfo_.begin();
                                               iter != metainfo_.end(); ++iter)
    {
        fieldNode.push_back(iter->Serialize());
    }

    return fieldNode;
}


void DataFieldInfo::Deserialize(const JSONNode& node)
{
    name_ = node.at("Name").as_string();
    type_ = node.at("ElementType").as_string();
    bytesPerElement_ = static_cast<int>(node.at("BytesPerElement").as_int());
    rank_ = static_cast<int>(node.at("Rank").as_int());

    // Get sizes
    iSize_ = static_cast<int>(node.at("ISize").as_int());
    jSize_ = static_cast<int>(node.at("JSize").as_int());
    kSize_ = static_cast<int>(node.at("KSize").as_int());
    if (rank_ >= 4) lSize_ = static_cast<int>(node.at("LSize").as_int());

    // Get halo sizes
    iMinusHalo_ = static_cast<int>(node.at("IMinusHaloSize").as_int());
    iPlusHalo_ = static_cast<int>(node.at("IPlusHaloSize").as_int());
    jMinusHalo_ = static_cast<int>(node.at("JMinusHaloSize").as_int());
    jPlusHalo_ = static_cast<int>(node.at("JPlusHaloSize").as_int());
    kMinusHalo_ = static_cast<int>(node.at("KMinusHaloSize").as_int());
    kPlusHalo_ = static_cast<int>(node.at("KPlusHaloSize").as_int());
    if (rank_ >= 4)
    {
        lMinusHalo_ = static_cast<int>(node.at("LMinusHaloSize").as_int());
        lPlusHalo_ = static_cast<int>(node.at("LPlusHaloSize").as_int());
    }
}

