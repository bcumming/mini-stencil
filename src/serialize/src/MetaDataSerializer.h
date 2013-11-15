// Created by Andrea Arteaga, MeteoSwiss
// Email: andyspiros@gmail.com
// January 2013

#pragma once

#include <string>

class JSONNode;
class DataFieldInfo;
class IndexTableEntry;
class MetaDataSerializerUnittest;

class MetaDataSerializer
{
    friend class Serializer;
    friend class MetaDataSerializerUnittest;

public:
    static JSONNode Serialize(const Serializer&);
    static JSONNode Serialize(const Serializer&, const DataFieldInfo&);
    static void Deserialize(Serializer&, const JSONNode&);
    static void Deserialize(Serializer&, const JSONNode&, const DataFieldInfo&);

private:

    // Serialization helper functions
    static JSONNode SerializeGlobalInfo(const Serializer&);

    // Deserialization helper functions
    static void DeserializeGlobalInfo(Serializer&, const JSONNode& node);
};


