// Created by Andrea Arteaga, MeteoSwiss
// Email: andyspiros@gmail.com
// January 2013

#pragma once

#include <vector>
#include <string>
#include <algorithm>
#include <iostream>
#include <sstream>
#include "SavePoint.h"

class JSONNode;

/**
 * Objects of this class represent an entry of the index table. Their are
 * basically savepoints with an associated list of fields that were stored
 * at the given savepoint and an offset in the corresponding file where the
 * binary data can be found.
 */
class IndexTableEntry
{
public:

    /**
     * Adds an entry to the list of fields that are saved at the associated
     * savepoint.
     *
     * @param fieldName The name of the field
     * @param offset The offset at which the field is saved
     */
    void AddField(std::string fieldName, size_t offset)
    {
        std::vector<std::string>::const_iterator findResult = fields_.begin(),
                                                 findEnd = fields_.end();

        // Check whether the name-offset pair has already been registered
        while(true)
        {
            findResult = find(findResult, findEnd, fieldName);

            // No result: register the new offset
            if (findResult == findEnd)
            {
                fields_.push_back(fieldName);
                offsets_.push_back(offset);
                return;
            }

            // Existing result: nothing to be done
            size_t i = static_cast<size_t>(findResult-fields_.begin());
            if (offsets_[i] == offset)
                return;
            ++findResult;
        }
    }

    /**
     * Joins the information of two table entries into the current one. The
     * information contained in both entries will not be duplicated.
     *
     * @param other The other entry containing information that must be
     *              included in the current object
     */
    void Merge(const IndexTableEntry& other)
    {
        const std::vector<std::string>& ofields = other.fields();
        const std::vector<size_t>& ooffsets = other.offsets();

        std::vector<std::string>::const_iterator iter = ofields.begin();
        for (int f = 0; iter != ofields.end(); ++iter, ++f)
        {
            AddField(*iter, ooffsets[f]);
        }
    }

    /**
     * Gives back whether the entry contains the given field
     *
     * @param fieldName The name of the field
     *
     * @returns True iff the field is stored at this savepoint
     */
    bool HasField(std::string fieldName) const
    {
        return
        std::find(fields_.begin(), fields_.end(), fieldName) != fields_.end();
    }


    /**
     * Gives back the offset relative to a field stored at this savepoint.
     *
     * @param fieldName The name of the field
     *
     * @returns an integer representing the offset of the binary data of the
     *          requested field or -1 if the requested field is not found
     */
    ptrdiff_t LookupFieldOffset(std::string fieldName, int instance=0) const
    {
        for (size_t i = 0; i < fields_.size(); ++i)
        {
            if (fields_[i] == fieldName && --instance < 0)
                // FIXME: what if offsets_[i] is not representable as ptrdiff_t?
                return static_cast<ptrdiff_t>(offsets_[i]);
        }

        return -1;
    }

    /**
     * Gives back the associated savepoint.
     *
     * @returns a constant reference to the associated savepoint
     */
    const SavePoint& savePoint() const
    {
        return savePoint_;
    }

    /**
     * Set the savepoint for this table entry.
     *
     * @param savePoint The savepoint to which the entry refers
     */
    void set_SavePoint(const SavePoint savePoint)
    {
        savePoint_ = savePoint;
    }

    /**
     * Gives back the list of fields that are stored at the associated savepoint
     *
     * @returns a constant reference to the list of field names
     */
    const std::vector<std::string>& fields() const
    {
        return fields_;
    }

    /**
     * Gives back the list of offsets at which the fields are stored in the
     * corresponding file.
     *
     * @returns a constant reference to the list of offsets
     */
    const std::vector<size_t>& offsets() const
    {
        return offsets_;
    }

    /**
     * Assemble the JSON node that describes the table entry. Used during the
     * serialization.
     *
     * @param id The ID of the entry as stored in the index table
     * @param fieldName The name of the field that is being serialized, or a
     *                  void string in case of single file mode
     *
     * @return The JSON node describing the entry
     */
    JSONNode Serialize(int id, std::string fieldName) const;

    /**
     * Interpret the information contained in the JSON node. Used during
     * the deserialization.
     *
     * @param node The JSON node containing the serialized information
     *
     * @return The ID of the table entry
     */
    int Deserialize(const JSONNode& node);


    /**
     * For debug purposes
     */
    std::string ToString() const
    {
        std::ostringstream strm;
        strm << savePoint_.ToString() << " { ";
        for (size_t i = 0; i < fields_.size(); ++i)
        {
            strm << fields_[i] << "(" << offsets_[i] << ") ";
        }
        strm << "}";
        return strm.str();
    }

private:
    SavePoint savePoint_;
    std::vector<std::string> fields_;
    std::vector<size_t> offsets_;
};


