// Created by Andrea Arteaga, MeteoSwiss
// Email: andyspiros@gmail.com
// January 2013

#pragma once

#include "SavePoint.h"

class Serializer;

class SerializerInput
{
public:
    /**
     * Default constructor
     */
    SerializerInput() { }

    /**
     * Copy constructor
     */
    SerializerInput(const SerializerInput& other)
    {
        *this = other;
    }

    /**
     * Assignment operator
     */
    SerializerInput& operator=(const SerializerInput& other)
    {
        pSerializer_ = other.pSerializer_;
        savePoint_ = other.savePoint_;
        return *this;
    }

    inline void Init(Serializer& serializer, std::string savePointName);

    inline void set_SavePoint(const SavePoint& other) { savePoint_ = other; }

    inline SerializerInput& operator>> (const MetaInfo& info);

    template<typename TDataField>
    inline SerializerInput& operator>> (TDataField& field);

private:
    SavePoint savePoint_;
    Serializer* pSerializer_;
};

