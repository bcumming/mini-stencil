// Created by Andrea Arteaga, MeteoSwiss
// Email: andyspiros@gmail.com
// January 2013

#pragma once

#include <string>
#include <vector>

#include "MetaInfo.h"

/**
 * This class represents a savepoint in the data file. A savepoint is
 * defined by a name and a list of MetaInfo objects. In order to uniquely
 * identify a savePoint, both the name and the set of metainformation is needed,
 * but the ordering of the MetaInfo objects is not important.
 */
class SavePoint
{
public:
    /**
     * Default constructor
     */
    SavePoint()
    { }

    /**
     * Copy constructor
     */
    SavePoint(const SavePoint& other)
    {
        *this = other;
    }

    /**
     * Assignment operator
     */
    SavePoint& operator=(const SavePoint& other)
    {
        name_ = other.name_;
        metainfo_ = other.metainfo_;
        return *this;
    }

    /**
     * Removes all the metainformation from the savepoint and cleans the name.
     */
    void clear()
    {
        name_.clear();
        metainfo_.clear();
    }


    /**
     * Sets the name for the savepoint
     *
     * @param name The new name for the savepoint
     */
    void set_Name(std::string name)
    {
        name_ = name;
    }


    /**
     * Adds a pair key-value.
     *
     * @param info An initialized MetaInfo object
     */
    void AddMetaInfo(const MetaInfo& info)
    {
        metainfo_.push_back(info);
    }

    /**
     * Adds a pair key-value. The type can be int, float, double or std::string
     *
     * @param key The key of the metainfo object
     * @param value The value of the metainfo object
     */
    template<typename TData>
    void AddMetaInfo(std::string key, TData value)
    {
        AddMetaInfo(meta_info(key, value));
    }


    /**
     * Comparison operator
     *
     * This operator checks whether two SavePoint objects represent the exactly
     * same savepoint. They do if the name matches, the amount of
     * metainformation is the same and every stored key-value pair is the same,
     * i.e. both the key and the value match (the order is not important).
     *
     * @returns True if and only if the savepoints are exactly equal
     */
    bool operator==(const SavePoint& other) const
    {
        // The names must match
        if (name_ != other.name_)
            return false;

        // The quantity of metainformation must be the same for both savepoints
        if (metainfo_.size() != other.metainfo_.size())
            return false;

        typedef std::vector<MetaInfo>::const_iterator iter_t;

        for(iter_t iter = metainfo_.begin(); iter != metainfo_.end(); ++iter)
        {
            bool found = false;

            // Find an entry in other's metainfo with same key
            for(iter_t jter = other.metainfo_.begin();
                       jter != other.metainfo_.end(); ++jter)
            {
                if(jter->key() == iter->key())
                {
                    // If the corresponding entry exists, the values must match
                    if(*jter == *iter)
                    {
                        found = true;
                        break;
                    }
                    else
                    {
                        return false;
                    }
                }
            }

            // If there is no corresponding entry, the savepoint do not match
            if (!found)
                return false;
        }

        return true;
    }

    bool operator!= (const SavePoint& other) const
    {
        return !(*this == other);
    }

    /**
     * @returns The name of the savepoint
     */
    std::string name() const
    {
        return name_;
    }

    MetaInfoType metaInfoType(std::string key) const
    {
        try
        {
            const MetaInfo& metaInfo = metaInfoReference(key);
            return metaInfo.type();
        }
        catch(MetaInfoNotFound&)
        {
            return cInvalid;
        }

        // Avoid warning
        return cInvalid;
    }

    /**
     * This method searches the metainformation for the key and gives in the
     * provided reference the related value, if it exists. Otherwise, it throws
     *
     * @param key The key that has to be found
     * @param value The reference where the value will be written
     */
    template<typename TValue>
    void metaInfoValue(std::string key, TValue& value) const
    {
        const MetaInfo& metaInfo = metaInfoReference(key);
        metaInfo.ExtractValue(value);
    }

    /**
     * @returns The vector containing the metainformation
     */
    const std::vector<MetaInfo>& metaInfo() const
    {
        return metainfo_;
    }

    /**
     * Writes the internal state of the savepoint into a string
     *
     * @returns The internal state as string
     */
    std::string ToString() const
    {
        std::ostringstream ss;
        ss << name_ << " [ ";

        typedef std::vector<MetaInfo>::const_iterator iter_t;
        for (iter_t iter = metainfo_.begin(); iter != metainfo_.end(); ++iter)
        {
            ss << iter->ToString() << " ";
        }

        ss << "]";

        return ss.str();
    }

private:
    const MetaInfo& metaInfoReference(std::string key) const
    {
        typedef std::vector<MetaInfo>::const_iterator iter_t;
        for (iter_t iter = metainfo_.begin(); iter != metainfo_.end(); ++iter)
        {
            if(iter->key() == key)
            {
                return *iter;
            }
        }

        std::string message("MetaInfo with key \"");
        message += key;
        message += "\" not found";
        throw MetaInfoNotFound(message);
    }

    std::string name_;
    std::vector<MetaInfo> metainfo_;
};


inline std::ostream& operator<<(std::ostream& out, const SavePoint& savePoint)
{
    out << savePoint.ToString();
    return out;
}
