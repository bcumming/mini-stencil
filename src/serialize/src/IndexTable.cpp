// Created by Andrea Arteaga, MeteoSwiss
// Email: andyspiros@gmail.com
// January 2013

#include "SerializationFramework.h"
#include <stdexcept>
#include <string>
#include <sstream>
#include "libjson.h"


void IndexTable::AddField(std::string fieldName, int savePointId,
                          const SavePoint& savePoint, size_t offset)
{
    // Throw in case of negative savepoint ID
    if (savePointId < 0)
    {
        std::ostringstream strm;
        strm << "Invalid savepoint ID: " << savePointId;
        throw std::invalid_argument(strm.str());
    }

    // Create new index table entry if ID is new
    if (entries_.count(savePointId) == 0)
    {
        entries_[savePointId] = IndexTableEntry();
        entries_[savePointId].set_SavePoint(savePoint);
    }

    // Add field to the index table entry
    entries_[savePointId].AddField(fieldName, offset);
}

size_t IndexTable::LookupFieldOffset(std::string fieldName,
        const SavePoint& savePoint, int instance) const
{
    ptrdiff_t offset = -1;
    int sID = -1;

    // Search the index table until the savepoint is found
    for (const_iterator iter = begin(); iter != end(); ++iter)
    {
        if (iter->second.savePoint() == savePoint)
        {
            sID = iter->first;
            offset = iter->second.LookupFieldOffset(fieldName, instance);
            break;
        }
    }

    // If the field was found at the savepoint, return its offset
    if (offset >= 0)
        return static_cast<size_t>(offset);

    // If the savepoint was not found, throw an exception
    if (sID < 0)
    {
        std::string message("The requested savepoint \"");
        message += savePoint.ToString() + "\" was not found";
        throw SerializationException(message);
    }

    // Otherwise, search back
    do
    {
        if (entries_.count(--sID))
        {
            offset = entries_.find(sID)->second.LookupFieldOffset(fieldName, instance);
        }
    } while(sID > 0 && offset < 0);

    // If found, return the offset
    if (offset >= 0)
        return static_cast<size_t>(offset);

    // If still not found, throw an exception
    std::string message("The field");
    message += fieldName + "is not defined at the requested savepoint \"";
    message += savePoint.ToString() + "\"";
    throw SerializationException(message);
}

SavePoint IndexTable::LookupSavePointBefore(std::string fieldName,
        const SavePoint& savePoint) const
{
    int sID = -1;

    // Search the index table until the savepoint is found
    for (const_iterator iter = begin(); iter != end(); ++iter)
    {
        if (iter->second.savePoint() == savePoint)
        {
            sID = iter->first;
            break;
        }
    }

    // Throw an exception if the savepoint has not been found
    if (sID < 0)
    {
        std::string message("The requested savepoint \"");
        message += savePoint.ToString() + "\" was not found";
        throw SerializationException(message);
    }

    // Throw an exception if the given savepoint is the first one
    if (sID == 0)
    {
        std::string message("The requested savepoint \"");
        message += savePoint.ToString();
        message += "\" has no preceding savepoint";
        throw SerializationException(message);
    }

    // Otherwise, search back
    do
    {
        if (entries_.count(--sID))
        {
            if (entries_.find(sID)->second.HasField(fieldName))
                return entries_.find(sID)->second.savePoint();
        }
    } while(sID > 0);

    // If still not found, throw an exception
    std::string message("The field ");
    message += fieldName + " is not defined before the requested savepoint \"";
    message += savePoint.ToString() + "\"";
    throw SerializationException(message);
}


JSONNode IndexTable::Serialize(std::string fieldName) const
{
    bool singleFile = (fieldName == "");

    JSONNode indexN;
    indexN.set_name("SavePoints");
    indexN.cast(JSON_ARRAY);

    // Add entries
    IndexTable::const_iterator entry = begin();
    for (int i = 0; entry != end(); ++entry, ++i)
    {
        // Check whether the field is saved at the savepoint
        if (singleFile || entry->second.HasField(fieldName))
        {
            // Wrapper around entry node
            JSONNode wrapN;
            wrapN.push_back(entry->second.Serialize(entry->first, fieldName));
            indexN.push_back(wrapN);
        }
    }

    return indexN;
}


void IndexTable::Deserialize(const JSONNode& node)
{
    for (JSONNode::const_iterator entryN = node.begin();
                                  entryN != node.end(); ++entryN)
    {
        IndexTableEntry entry;
        int entryID = entry.Deserialize((*entryN).at(0));

        if (entries_.count(entryID))
            entries_[entryID].Merge(entry);
        else
            entries_[entryID] = entry;
    }

}
















