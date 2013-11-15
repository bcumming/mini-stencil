// Created by Andrea Arteaga, MeteoSwiss
// Email: andyspiros@gmail.com
// January 2013

#pragma once

#include <map>
#include <sstream>
#include <iomanip>
#include <cmath>
#include "IndexTableEntry.h"
#ifdef USE_STELLA
    #include "Definitions.h"
#else
    #include "Util.h"
#endif

class JSONNode;

class IndexTable
{
    DISALLOW_COPY_AND_ASSIGN(IndexTable);

public:
    /**
     * Default constructor
     */
    IndexTable()
    { }

    // Typedefs (according to std::map)
    typedef std::map<int, IndexTableEntry>::iterator iterator;
    typedef std::map<int, IndexTableEntry>::const_iterator const_iterator;
    typedef IndexTableEntry& reference;
    typedef const IndexTableEntry& const_reference;

    // std::map wrapper functions
    iterator begin() { return entries_.begin(); }
    const_iterator begin() const { return entries_.begin(); }

    iterator end() { return entries_.end(); }
    const_iterator end() const { return entries_.end(); }

    reference front() { return entries_.begin()->second; }
    const_reference front() const { return entries_.begin()->second; }

    reference back() { return entries_.rbegin()->second; }
    const_reference back() const { return entries_.rbegin()->second; }

    void clear() { entries_.clear(); }

    void push_back(const IndexTableEntry& entry)
    {
        int sID = 0;
        if (!entries_.empty())
            sID = entries_.rbegin()->first+1;
        entries_[sID] = entry;
    }

    size_t size() const { return entries_.size(); }


    /**
     * Adds a new reference of the given field to the given savepoint and with
     * the given offset to the corresponding storage file.
     */
    void AddField(std::string fieldName, int savePointId,
                  const SavePoint& savePoint, size_t offset);


    /**
     * Gives back the offset relative to a field stored at the given savepoint.
     *
     * @param fieldName The name of the field
     *
     * @param savePoint The savepoint at which the field is stored
     * @param instance The instance of the field at the save point to select
     *
     * @returns an integer representing the offset of the binary data of the
     *          requested field, -1 if the requested field is not found at
     *          the given savepoint or -2 if the requested savepoint is not
     *          found.
     */
    size_t LookupFieldOffset(std::string fieldName,
                             const SavePoint& savePoint, int instance) const;

    /**
     * Searches for the nearest preceding savepoint at which the given field
     * was saved
     *
     * @param fieldName The name of the desired field
     * @param savePoint The savepoint before which the other savepoint must
     *                  be found
     */
    SavePoint LookupSavePointBefore(std::string fieldName,
                                    const SavePoint& savePoint) const;

    /**
     * Assembles the JSON node that describes the table. Used during the
     * serialization.
     *
     * @param fieldName The name of the field that is being serialized, or a
     *                  void string in case of single file mode (default)
     *
     * @return The JSON node describing the entry
     */
    JSONNode Serialize(std::string fieldName="") const;

    /**
     * Interpret the information contained in the JSON node. Used during
     * the deserialization.
     *
     * @param node The JSON node containing the serialized information
     */
    void Deserialize(const JSONNode& node);

    /**
     * For debug purposes
     */
    std::string ToString() const
    {
        if (entries_.empty())
            return " ** No savepoints\n";
        
        int maxIndex = entries_.rbegin()->first;
        int w = static_cast<int>(std::floor(std::log10(static_cast<float>(maxIndex))+1));

        std::ostringstream strm;
        for (const_iterator iter = entries_.begin();
                            iter != entries_.end(); ++iter)
        {
            strm << "  -- " << std::setw(w) << iter->first << " | "
                 << iter->second.ToString() << "\n";
        }
        return strm.str();
    }

private:
    std::map<int, IndexTableEntry> entries_;
};

