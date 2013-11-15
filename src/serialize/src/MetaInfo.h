// Created by Andrea Arteaga, MeteoSwiss
// Email: andyspiros@gmail.com
// January 2013

#pragma once

/*
 * MetaInfo.h
 *
 *  Created on: 05.11.2012
 *      Author: Andrea Arteaga
 */

#include <string>
#include <sstream>
#include <iomanip>
#include <cassert>
#include <iostream>
#include <cstdlib>
#include "SerializationExceptions.h"

class JSONNode;

enum MetaInfoType
{
    cInteger,
    cFloatSingle,
    cFloatDouble,
    cString,
    cInvalid
};

/**
 * Objects of this class hold a single piece of metainformation provided as
 * a pair key-value. The key is a string, while the value can be an integer,
 * a floating point number or a string.
 */
class MetaInfo
{
public:
    /**
     * Default constructor
     */
    MetaInfo()
    {
        key_ = "";
        valueInt_ = 0;
        valueFloat_ = 0.f;
        valueDouble_ = 0.;
        valueString_ = "";
        type_ = cInvalid;
    }

    /**
     * Copy constructor
     */
    MetaInfo(const MetaInfo& other)
    {
        *this = other;
    }

    /**
     * Assignment operator
     */
    MetaInfo& operator=(const MetaInfo& other)
    {
        key_ = other.key_;
        valueInt_ = other.valueInt_;
        valueFloat_ = other.valueFloat_;
        valueDouble_ = other.valueDouble_;
        valueString_ = other.valueString_;
        type_ = other.type_;

        return *this;
    }

    /**
     * Initializer with integer value.
     *
     * @param key The string representing the key of the metainformation
     * @param value The value associated with the given key
     */
    void Init(std::string key, int value)
    {
        key_ = key;
        type_ = cInteger;
        valueInt_ = value;
    }

    /**
     * Initializer with single precision floating point value.
     *
     * @param key The string representing the key of the metainformation
     * @param value The value associated with the given key
     */
    void Init(std::string key, float value)
    {
        key_ = key;
        type_ = cFloatSingle;
        valueFloat_ = value;
    }

    /**
     * Initializer with double precision floating point value.
     *
     * @param key The string representing the key of the metainformation
     * @param value The value associated with the given key
     */
    void Init(std::string key, double value)
    {
        key_ = key;
        type_ = cFloatDouble;
        valueDouble_ = value;
    }

    /**
     * Initializer with string value.
     *
     * @param key The string representing the key of the metainformation
     * @param value The value associated with the given key
     */
    void Init(std::string key, std::string value)
    {
        key_ = key;
        type_ = cString;
        valueString_ = value;
    }

    /**
     * Gives back the key as string
     */
    std::string key() const
    {
        return key_;
    }

    /**
     * Gives back the type of the metainfo
     */
    MetaInfoType type() const
    {
        return type_;
    }

    /**
     * Checks whether two metainformation objects represent the same
     * key-value pair. This method checks the key, the type of the stored
     * information and the value. If at least one of these controls fails, the
     * parameter is considered different from the current object.
     *
     * @param other The MetaInfo object against which the current object must
     *              be compared
     *
     * @returns True if and only if the parameter is completely equal to the
     *          current object
     */
    bool operator==(const MetaInfo& other) const
    {
        // Key match
        if (key_ != other.key_)
            return false;

        // Type match
        if (type_ != other.type_)
            return false;

        // Value match
        switch (type_)
        {
        case cInteger:
            return valueInt_ == other.valueInt_;
        case cFloatSingle:
            return valueFloat_ == other.valueFloat_;
        case cFloatDouble:
            return valueDouble_ == other.valueDouble_;
        case cString:
            return valueString_ == other.valueString_;
        case cInvalid:
            std::cerr << "Cannot compare an invalid MetaInfo object\n";
            assert(false);
            exit(-1);
        }

        // Avoid warning
        return false;
    }

    /**
     * Checks whether two metainformation objects differ. The definition of
     * equality is given for the operator==.
     *
     * @return true iff operator== returns false on the same objects.
     */
    bool operator!=(const MetaInfo& other) const
    {
        return !(*this == other);
    }


    /**
     * Serializes the key-value pair as JSON node
     *
     * @returns a valid JSONNode object representing the information stored
     *          in the object
     */
    JSONNode Serialize() const;

    /**
     * Given a valid JSONNode whose value is supported by this class (int,
     * float, double or string), this method changes the key and values
     * currently stored in the object with the ones present in the node.
     * The name of the node is used as key, while the value, if representing
     * a number, is stored as int if it is effectively an integer and as
     * double otherwise.
     *
     * @param node The JSON node containing the information that must be
     *             interpreted
     */
    void Deserialize(const JSONNode& node);

    template<typename TValue>
    void ExtractValue(TValue& value) const
    {
        std::stringstream stream;
        switch(type_)
        {
        case cInteger:
            value = static_cast<TValue>(valueInt_);
            break;
        case cFloatSingle:
            value = static_cast<TValue>(valueFloat_);
            break;
        case cFloatDouble:
            value = static_cast<TValue>(valueDouble_);
            break;
        case cString:
            stream << valueString_;
            stream >> value;
            break;
        case cInvalid:
            throw BadMetaInfo("Trying to read an invalid MetaInfo");
        }
    }

    void ExtractValue(std::string& value) const
    {
        std::ostringstream stream;
        switch(type_)
        {
        case cInteger:
            stream << valueInt_;
            break;
        case cFloatSingle:
            stream << valueFloat_;
            break;
        case cFloatDouble:
            stream << valueDouble_;
            break;
        case cString:
            stream << valueString_;
            break;
        case cInvalid:
            throw BadMetaInfo("Trying to read an invalid MetaInfo");
        }
        value = ValueToString();
    }

    /**
     * Writes the current value as string.
     *
     * @returns The string representing the current value, independently of the
     *          actual type
     */
    std::string ValueToString() const
    {
        std::ostringstream ss;

        switch(type_)
        {
        case cInteger:
            ss << valueInt_;
            break;
        case cFloatSingle:
            ss << valueFloat_;
            break;
        case cFloatDouble:
            ss << valueDouble_;
            break;
        case cString:
            ss << valueString_;
            break;
        case cInvalid:
            ss << "Invalid MetaInfo object";
            break;
        }

        return ss.str();
    }

    /**
     * Writes a representation of the inner state.
     *
     * @return a string representing the inner state, i.e. the key and value
     *         representations
     */
    std::string ToString() const
    {
        return key_ + "=" + ValueToString();
    }

private:
    std::string key_;

    int valueInt_;
    float valueFloat_;
    double valueDouble_;
    std::string valueString_;

    MetaInfoType type_;
};


/**
 * This function implements the output operator so that it will be possible
 * to write a MetaInfo directly into an output stream.
 */
inline std::ostream& operator<<(std::ostream& out, const MetaInfo& info)
{
    out << info.ToString();
    return out;
}



/**
 * Constructs a MetaInfo object out of a key-value pair. Accepts integers,
 * floating point numbers and strings as values.
 *
 * @param key The key of the MetaInfo object
 * @param key The value of the MetaInfo object
 * @returns The MetaInfo object
 */
template<typename TData>
inline MetaInfo meta_info(std::string key, TData value)
{
    MetaInfo info;
    info.Init(key, value);
    return info;
}

