// Created by Andrea Arteaga, MeteoSwiss
// Email: andyspiros@gmail.com
// January 2013

#include "SerializationFramework.h"
#include "libjson.h"

JSONNode MetaInfo::Serialize() const
{
    JSONNode node;
    node.set_name(key_);

    switch(type_)
    {
    case cInteger:
        node = valueInt_;
        break;
    case cFloatSingle:
        node = valueFloat_;
        break;
    case cFloatDouble:
        node = valueDouble_;
        break;
    case cString:
        node = valueString_;
        break;
    case cInvalid:
        throw BadMetaInfo("Trying to serialize an invalid MetaInfo");
    }

    return node;
}


void MetaInfo::Deserialize(const JSONNode& node)
{
    // Key
    key_ = node.name();

    // Check type
    double value;
    switch(node.type())
    {
    case JSON_NUMBER:
        value = node.as_float();
        if (value == static_cast<double>(static_cast<int>(value)))
        {
            type_ = cInteger;
            valueInt_ = static_cast<int>(value);
        } else
        {
            type_ = cFloatDouble;
            valueDouble_ = value;
        }
        break;
    case JSON_STRING:
        type_ = cString;
        valueString_ = node.as_string();
        break;
    default:
        throw std::invalid_argument("Node does not contain a valid metainfo");
    }
}
