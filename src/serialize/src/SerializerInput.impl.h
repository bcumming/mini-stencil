// Created by Andrea Arteaga, MeteoSwiss
// Email: andyspiros@gmail.com
// January 2013

#pragma once

void SerializerInput::Init(Serializer& serializer, std::string savePointName)
{
    pSerializer_ = &serializer;
    savePoint_.set_Name(savePointName);
}

SerializerInput& SerializerInput::operator>> (const MetaInfo& info)
{
    savePoint_.AddMetaInfo(info);
    return *this;
}

template<typename TDataField>
SerializerInput& SerializerInput::operator>> (TDataField& field)
{
    pSerializer_->Load(field, savePoint_);
    return *this;
}
