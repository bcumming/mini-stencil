// Created by Andrea Arteaga, MeteoSwiss
// Email: andyspiros@gmail.com
// January 2013

#pragma once

#include <string>
#include <vector>
#include <map>
#include <fstream>
#include <sstream>
#include <cassert>
#include <boost/static_assert.hpp>
#ifdef USE_STELLA
    #include "Definitions.h"
    #include "SharedInfrastructure.h"
#else
    #include "Util.h"
#endif

class Serializer
{
    DISALLOW_COPY_AND_ASSIGN(Serializer);

    friend class MetaDataSerializer;

public:
    /**
     * Default constructor
     */
    Serializer()
    : singleFile_(false), readwrite_(false), versionMajor_(1), versionMinor_(0),
      author_("SerializationFramework"), basepath_(""), basename_(""),
      initialized_(false)
    {
    }

    /**
     * Initializes the serializer.
     *
     * @param singleFile If set, the fields will be written and read to/from a
     *                   single file containing data for multiple fields.
     *                   Otherwise, a different data file and a metadata file
     *                   will be written/read for every field.
     * @param readwrite If set, the data present on disk will be removed as
     *                  soon as the corresponding field is used with the
     *                  serializer. Otherwise, the files are open in read-only
     *                  mode and no write operation is allowed.
     * @param basepath The path where the files will be searched and saved. It
     *                 defaults to the current path given in POSIX format. The
     *                 directory must exist and be at least readable (writable
     *                 if write operations are to be performed).
     * @param basename The basename of the file to be read. In single file mode
     *                 this corresponds to the filename without postfix. In multi
     *                 file mode, this corresponds to the base filename without
     *                 the underscore, field name and postfix.
     */
    void Init(bool singleFile, bool readwrite,
              std::string basepath = "", std::string basename = "")
    {

        if (initialized_)
        {
            throw SerializationException(
                    "The serializer cannot be initialized twice");
        }

        if (basepath.empty()) basepath = defaultBasepath();
        if (basename.empty()) basename = defaultBasename();
        if (*basepath.rbegin() != '/')
            basepath.push_back('/');

        singleFile_ = singleFile;
        readwrite_ = readwrite;
        basepath_ = basepath;
        basename_ = basename;
        initialized_ = true;
    }

    /**
     * Clears completely the internal state, unregistering all fields and
     * clearing the index table. All information will be lost. Do not use
     * this method if you are not completely sure that this is what you want!
     */
    void clear()
    {
        fieldInfos_.clear();
        indexTable_.clear();
        globalMetainfo_.clear();
        lastSavePoint_.clear();
        basepath_ = defaultBasepath();
        basename_ = defaultBasename();
        initialized_ = false;
    }

    /**
     * This static function returns the default base path that is used when
     * the corresponding argument is not passed at initialization time.
     */
    static std::string defaultBasepath() { return "./"; }

    /**
     * This static function returns the default base file name that is used when
     * the corresponding argument is not passed at initialization time.
     */
    static std::string defaultBasename() { return "Field"; }

    /**
     * Set the value for the version information, which is present in the
     * "GlobalInfo" tag at the beginning of every file. The version is
     * written as "A.B", where A and B are integer numbers and A is called
     * versionMajor, while B is called versionMinor.
     *
     * The default value for this is "1.0".
     *
     * @param versionMajor The number present before the dot
     * @param versionMinor The number present after the dot
     */
    void set_Version(int versionMajor, int versionMinor)
    {
        versionMajor_ = versionMajor;
        versionMinor_ = versionMinor;
    }

    /**
     * Set the name of the author of the files. Default value for this is
     * "SerializationFramework".
     *
     * @param author The name of the author
     */
    void set_Author(std::string author)
    {
        author_ = author;
    }

    /**
     * Gives read-only access to the index table
     */
    const IndexTable& indexTable() const { return indexTable_; }

    /**
     * Gives read-only access to the set of registered DataFieldInfo objects
     */
    const std::map<std::string, DataFieldInfo>& fieldInfos() const
    {
        return fieldInfos_;
    }

    /**
     * Tells whether the serializer has been initialized
     */
    bool initialized() const { return initialized_; }

    /**
     * Tells whether the serializer is in single-file mode
     */
    bool singleFile() const { return singleFile_; }

    /**
     * Tells whether the serializer is open in read-write mode
     */
    bool readWrite() const { return readwrite_; }

    /**
     * Gives the base path
     */
    std::string basepath() const { return basepath_; }

    /**
     * Gives the base name
     */
    std::string basename() const { return basename_; }


    /**
     * This operator initializes the output operation: the desired name for the
     * savepoint is passed, and the serializer modifies its internal state so
     * that it is ready for accepting metainformation regarding the savepoint
     * and the the fields that must be stored.
     *
     * @param savePointName The name of the savepoint that must be created
     *
     * @returns an object that allows to add metainformation for the savepoint
     *          and to write data fields
     */
    SerializerOutput operator<<(std::string savePointName)
    {
        SerializerOutput ser;
        ser.Init(*this, savePointName);
        return ser;
    }



    /**
     * This operator initializes the input operation: the desired name for the
     * savepoint is passed, and the serializer modifies its internal state so
     * that it is ready for accepting metainformation regarding the savepoint
     * and the the fields that must be loaded.
     *
     * @param savePointName The name of the savepoint that must be found
     *
     * @returns an object that allows to add metainformation for the savepoint
     *          and to load data fields
     */
    SerializerInput operator>>(std::string savePointName)
    {
        SerializerInput ser;
        ser.Init(*this, savePointName);
        return ser;
    }


    /**
     * This operator stores the provided data field at the last used savepoint
     * for output operations.
     *
     * @param field The field that must be stored
     *
     * @returns an object that allows to store more data fields
     */
#ifdef USE_STELLA
    template<
        typename TStorage,
        typename TDataFieldImpl
    >
    SerializerOutput operator<<(const DataField<TStorage,TDataFieldImpl>& field)
    {
        SerializerOutput ser;
        ser.Init(*this, lastSavePoint_.name());
        ser.set_SavePoint(lastSavePoint_);
        return ser << field;
    }

    template<
        typename TStorage,
        typename TDataFieldImpl
    >
    SerializerInput operator>>(const DataField<TStorage, TDataFieldImpl>& field)
    {
        SerializerInput ser;
        ser.Init(*this, lastSavePoint_.name());
        ser.set_SavePoint(lastSavePoint_);
        return ser >> field;
    }
#endif

    /**
     * This method adds a key-value pair to the list of global metainformation.
     *
     * @param info The initialized object containing the key-value pair
     */
    void AddGlobalMetaInfo(const MetaInfo& info);


    /**
     * This method adds a key-value pair to the list of metainformation for a
     * given data field.
     *
     * @param info The initialized object containing the key-value pair
     */
    void AddFieldMetaInfo(std::string fieldName, const MetaInfo& info);

    /**
     * This function tries to read the data file corresponding to the field
     * that has the name given as parameter. If the file is found, the field
     * information is imported; otherwise, a SerializerException is thrown.
     *
     * @param name The name of the field
     */
    void ImportField(std::string fieldName);

    /**
     * Load the metadata for all fields and savepoints in case of dsingle file
     * mode.
     */
    void LoadMetadata(std::string fieldName);

    /**
     * Method returning a list of save points that match the name passed by argument
     * @param fieldName name of the field
     * @param savePointName name of save point
     */
    std::vector<const SavePoint*> FindSavePointByName(std::string fieldName, std::string savePointName);

    /**
     * Method returning a list of save points that match the name passed by argument.
     * Assumes the savePoint is saved in a field with the same name
     * @param savePointName name of save point
     */
    std::vector<const SavePoint*> FindSavePointByName(std::string savePointName);

#ifdef USE_STELLA
    /**
     * This function initializes the given field so that it is prepared to
     * perform input operations. The field must already have a name: it will
     * not be modified.
     *
     * @param field The field that has to be initialized
     */
    template<typename TDataField>
    void InitField(TDataField& field);

    /**
     * This function initializes the given field so that it is prepared to
     * perform input operations.
     *
     * @param field The field that has to be initialized
     * @param name The name that the field must be initialized with
     */
    template<typename TDataField>
    void InitField(TDataField& field, std::string name);
#endif


    /**
     * This function gives back the number of instancies of a field in a
     * given save point.
     *
     * @param fieldName The name of the field that must be found
     * @param savePoint The save point at which the instancies have to be found
     *
     * @return The number of instancies of a field saved at the given save point
     */
    int InstancesAtSavePoint(std::string fieldName, const SavePoint& savePoint) const
    {
        const std::vector<std::string>& f = findSavePoint(savePoint).fields();
        return static_cast<int>(std::count(f.begin(), f.end(), fieldName));
    }


    /**
     * This function registers into the serializer a field with given name,
     * size and size of halo. This is mainly used in the wrapper for Fortran
     * and can also be useful when working without the shared infrastructure.
     *
     * @param fieldName The name of the field
     * @param iSize The size of the calculation domain in I direction
     * @param jSize The size of the calculation domain in J direction
     * @param kSize The size of the calculation domain in K direction
     * @param lSize The size of the calculation domain in L direction
     * @param iMinusHalo The size of the halo in negative I direction
     * @param iPlusHalo The size of the halo in positive I direction
     * @param jMinusHalo The size of the halo in negative J direction
     * @param jPlusHalo The size of the halo in positive J direction
     * @param kMinusHalo The size of the halo in negative K direction
     * @param kPlusHalo The size of the halo in positive K direction
     * @param lMinusHalo The size of the halo in negative L direction
     * @param lPlusHalo The size of the halo in positive L direction
     */
    template<typename TValue>
    void RegisterField(std::string fieldName,
                              int iSize, int jSize, int kSize, int lSize,
                              int iMinusHalo, int iPlusHalo,
                              int jMinusHalo, int jPlusHalo,
                              int kMinusHalo, int kPlusHalo,
                              int lMinusHalo, int lPlusHalo);

#ifdef USE_STELLA
    /**
     * Serializes a field by appending its binary data to the DAT file and
     * writing its metadata into the JSON file. The name of the serialized field
     * will be the same that is contained in the field object.
     *
     * @param field The field containing the data to serialize
     * @param savePoint The save point at which the field is serialized
     */
    template<typename TDataField>
    void Save(const TDataField& field, const SavePoint& savePoint);

    /**
     * Serializes a field by appending its binary data to the DAT file and
     * writing its metadata into the JSON file.
     *
     * @param field The field containing the data to serialize
     * @param savePoint The save point at which the field is serialized
     * @param name The name of the serialized field
     */
    template<typename TDataField>
    void Save(const TDataField& field, const SavePoint& savePoint,
                     std::string fieldName);
#endif

    /**
     * Serializes a field by appending its binary data to the DAT file and
     * writing its metadata into the JSON file. This function is mainly used in
     * Fortran wrapper and can be useful when used without the shared
     * infrastructure. Before calling this function, the field must have been
     * registered with the function Serializer::RegisterField.
     *
     * @param fieldName The name of the registered field
     * @param data The array containing the data of the field. The data must be
     *             placed in a column-major (Fortran-like) layout
     * @param savePoint The save point at which the field is serialized
     */
    template<typename TData>
    void Save(std::string fieldName, const TData* data,
                     const SavePoint& savePoint);


#ifdef USE_STELLA
    /**
     * Loads from file a field and stores the data into the given field. The
     * name of the field object will be used as name of the field to
     * deserialize. The field object must be initialized with the correct size.
     * If the field is not found at the given save point, the Serializer will
     * search back until it finds an occurrence, or throw a
     * SerializationException if none is found.
     *
     * @param field The field object that will be filled with data
     * @param savePoint The save point at which the data must be loaded
     * @param instance The instance of the field at the save point to select
     */
    template<typename TDataField>
    void Load(TDataField& field, const SavePoint& savePoint, int instance=0);

    /**
     * Loads from file a field and stores the data into the given field. The
     * field object must be initialized with the correct size. If the field is
     * not found at the given save point, the Serializer will search back until
     * it finds an occurrence, or throw a SerializationException if none is
     * found.
     *
     * @param field The field object that will be filled with data
     * @param savePoint The save point at which the data must be loaded
     * @param fieldName The name of the field to deserialize
     * @param instance The instance of the field at the save point to select
     */
    template<typename TDataField>
    void Load(TDataField& field, const SavePoint& savePoint,
                     std::string fieldName, int instance=0);
#endif

    /**
     * Loads from file a field and stores the data into the given array. The
     * array must be big enough to contain both the calculation domain and the
     * boundaries. The field must have been registered with
     * Serializer::RegisterField before using this function. If the field is
     * not found at the given save point, the Serializer will search back until
     * it finds an occurrence, or throw a SerializationException if none is
     * found.
     *
     * @param fieldName The name of the field to deserialize
     * @param data The array where the data will be written
     * @param savePoint The save point at which the data must be loaded
     * @param instance The instance of the field at the save point to select
     */
    template<typename TData>
    void Load(std::string fieldName, TData* data,
                     const SavePoint& savePoint, int instance=0);


#ifdef USE_STELLA
    /**
     * Loads from file before a certain save point a field and stores the data
     * into the given field. The field object must be initialized with the
     * correct size. The last suitable save point before the given one will be
     * chosen. The name of the field object is used as name of the field to
     * deserialize.
     *
     * @param field The field object that will be filled with data
     * @param savePoint The save point before which the data must be loaded
     */
    template<typename TDataField>
    void LoadBefore(TDataField& field, const SavePoint& savePoint);

    /**
     * Loads from file before a certain save point a field and stores the data
     * into the given field. The field object must be initialized with the
     * correct size. The last suitable save point before the given one will be
     * chosen.
     *
     * @param field The field object that will be filled with data
     * @param savePoint The save point before which the data must be loaded
     * @param fieldName The name of the field to deserialize
     */
    template<typename TDataField>
    void LoadBefore(TDataField& field, const SavePoint& savePoint,
                           std::string name);
#endif


    // Helper function to retrieve a DataFieldInfo
    const DataFieldInfo& GetFieldInfo(std::string fieldName);

    /**
     * This function adds to the list of field infos the given object. If
     * a DataFieldInfo for the same field name is already present, the function
     * checks whether the information is consistent. Otherwise, an exception is
     * thrown.
     *
     * @param fieldInfo The DataFieldInfo object that must be added to the list
     */
    void AddFieldInfo(const DataFieldInfo& fieldInfo)
    {
        const std::string name = fieldInfo.name();
        if (fieldInfos_.count(name))
        {
            if (fieldInfos_[name] != fieldInfo)
            {
                std::string message = "Requested field '";
                message.append(name);
                message.append("' is already registered, but specifications");
                message.append(" do not match");
                throw BadFieldInfo(message);
            }
        }
        else
        {
            // Field not yet registered: append it
            fieldInfos_[name] = fieldInfo;
        }
    }

    /**
     * For debug purposes
     */
    std::string ToString() const
    {
        std::ostringstream strm;

        if (fieldInfos_.empty())
        {
            strm << " *** No fields\n";
        }
        else for (std::map<std::string, DataFieldInfo>::const_iterator
                  iter = fieldInfos_.begin(); iter != fieldInfos_.end(); ++iter)
        {
            strm << " ** " << iter->second.ToString() << "\n";
        }
        
        strm << "\n\n" << indexTable_.ToString() << "\n";

        return strm.str();
    }

private:
    std::string fileBaseName(std::string fieldName) const
    {
        if(singleFile_ || fieldName.empty())
        {
            return basepath_ + basename_;
        }
        else
        {
            return basepath_ + basename_ + '_' + fieldName;
        }
    }

    const IndexTableEntry& findSavePoint(const SavePoint& savePoint) const
    {
        for (IndexTable::const_iterator iter = indexTable_.begin();
                iter != indexTable_.end(); ++iter)
        {
            if (iter->second.savePoint() == savePoint)
                return iter->second;
        }

        std::string message("Save point \"");
        message += savePoint.ToString() + "\" not found in index table";
        throw SerializationException(message);
    }

    // Prepares the field to be read/written
    size_t InitializeOutput(const DataFieldInfo&, const SavePoint&);
    void InitializeInput(std::string fieldName);
    size_t GetInputSavePointOffset(std::string, const SavePoint&, int instance);

    // Global meta information
    bool singleFile_;
    bool readwrite_;
    int versionMajor_;
    int versionMinor_;
    std::string author_;
    std::vector<MetaInfo> globalMetainfo_;

    // Tables
    std::map<std::string, DataFieldInfo> fieldInfos_;
    IndexTable indexTable_;

    // Internal state
    std::string basepath_;
    std::string basename_;
    bool initialized_;
    SavePoint lastSavePoint_;
};




///////////////////////////////////////////////////////////////////////////////
//////////////// IMPLEMENTATION OF INLINE AND TEMPLATE METHODS ////////////////
///////////////////////////////////////////////////////////////////////////////



template<typename TValue>
void Serializer::RegisterField(std::string fieldName,
                               int iSize, int jSize, int kSize, int lSize,
                               int iMinusHalo, int iPlusHalo,
                               int jMinusHalo, int jPlusHalo,
                               int kMinusHalo, int kPlusHalo,
                               int lMinusHalo, int lPlusHalo)
{
    bool newField = (fieldInfos_.count(fieldName) == 0);

    // Truncate existing files if we are in read-write mode
    if (newField && readwrite_)
    {
        std::fstream fsMeta((fileBaseName(fieldName) + ".json").c_str(),
                            std::fstream::out | std::fstream::trunc);
        std::fstream fsData((fileBaseName(fieldName) + ".dat").c_str(),
                            std::fstream::out | std::fstream::trunc);
    }
    else if (newField)
    {
        // Try to import the field, but do not throw if the file does not exist
        try
        {
            ImportField(fieldName);
        }
        catch(FieldNotFound&)
        {
        }

        newField = (fieldInfos_.count(fieldName) == 0);
    }

    // If in read-only mode, the field could have just been imported
    if (!newField)
    {
        DataFieldInfo& other = fieldInfos_[fieldName];
        if (
                   other.iSize() != iSize
                || other.jSize() != jSize
                || other.kSize() != kSize
                || other.lSize() != lSize
                || other.iPlusHaloSize() != iPlusHalo
                || other.jPlusHaloSize() != jPlusHalo
                || other.kPlusHaloSize() != kPlusHalo
                || other.lPlusHaloSize() != lPlusHalo
                || other.iMinusHaloSize() != iMinusHalo
                || other.jMinusHaloSize() != jMinusHalo
                || other.kMinusHaloSize() != kMinusHalo
                || other.lMinusHaloSize() != lMinusHalo
        )
        {
            std::cerr << "Field '" << fieldName << "' already registered, but "
                      << "size or halo size do not match";
            assert(false);
            exit(-1);
        }

        return;
    }

    // Retrieve field information
    std::string type = type_name<TValue>();
    int bytesPerElement = sizeof(TValue);
    int rank = 0;
    if (iSize > 1) ++rank;
    if (jSize > 1) ++rank;
    if (kSize > 1) ++rank;
    if (lSize > 1) ++rank;

    // Construct the data field info object
    DataFieldInfo fieldInfo;
    fieldInfo.Init(fieldName, type, bytesPerElement, rank,
                   iSize, jSize, kSize, lSize,
                   iMinusHalo, iPlusHalo,
                   jMinusHalo, jPlusHalo,
                   kMinusHalo, kPlusHalo,
                   lMinusHalo, lPlusHalo);

    // Store the data field info object
    fieldInfos_[fieldName] = fieldInfo;
}

#ifdef USE_STELLA
template<typename TDataField>
void Serializer::InitField(TDataField& field)
{
    BOOST_STATIC_ASSERT(is_data_field<TDataField>::value);

    if (field.name().empty())
        throw SerializationException("Trying to initialize an unnamed field");
    InitField(field, field.name());
}


template<typename TDataField>
void Serializer::InitField(TDataField& field, std::string fieldName)
{
    BOOST_STATIC_ASSERT(is_data_field<TDataField>::value);

    const DataFieldInfo& info = GetFieldInfo(fieldName);

    KBoundary kboundary;
    kboundary.Init(info.kMinusHaloSize(), info.kPlusHaloSize());

    field.Init(fieldName, info.calculationDomain(), kboundary);
}


template<typename TDataField>
void Serializer::Save(const TDataField& field, const SavePoint& savePoint)
{
    BOOST_STATIC_ASSERT(is_data_field<TDataField>::value);

    Serializer::Save(field, savePoint, field.name());
}

template<typename TDataField>
void Serializer::Save(const TDataField& field, const SavePoint& savePoint,
                      std::string fieldName)
{
    BOOST_STATIC_ASSERT(is_data_field<TDataField>::value);
    
    DataFieldInfo fieldInfo;
    fieldInfo.Init(field, fieldName);
    assert(fieldInfo.bytesPerElement() == sizeof(typename TDataField::ValueType));

    // Serialize metadata and get write offset
    size_t offset = InitializeOutput(fieldInfo, savePoint);

    // Open binary file for appending the data
    std::string fname = fileBaseName(fieldInfo.name()) + ".dat";
    std::fstream fsDat(fname.c_str(),
          std::ios_base::out | std::ios_base::app | std::ios_base::binary);

    // Store offset of the beginning of the field's binary data
    fsDat.seekp(static_cast<std::streamoff>(offset), std::ios_base::beg);

    // Compute indices ends
    const IJKSize& domain = fieldInfo.calculationDomain();
    const int iStart = -fieldInfo.iMinusHaloSize();
    const int jStart = -fieldInfo.jMinusHaloSize();
    const int kStart = -fieldInfo.kMinusHaloSize();
    const int iEnd = domain.iSize() + fieldInfo.iPlusHaloSize();
    const int jEnd = domain.jSize() + fieldInfo.jPlusHaloSize();
    const int kEnd = domain.kSize() + fieldInfo.kPlusHaloSize();

    // Append binary data at the end of the file
    typename TDataField::ValueType value;
    char *value_c = reinterpret_cast<char*>(&value);
    for (int i = iStart; i < iEnd; ++i)
        for (int j = jStart; j < jEnd; ++j)
            for (int k = kStart; k < kEnd; ++k)
            {
                value = field(i, j, k);
                fsDat.write(value_c, fieldInfo.bytesPerElement());
            }
    fsDat.close();
}
#endif

template<typename TData>
void Serializer::Save(std::string fieldName, const TData* data,
                      const SavePoint& savePoint)
{
    // Check whether the field was registered
    if(fieldInfos_.count(fieldName) == 0)
    {
        std::string message("Saving the field '");
        message += fieldName + "' by pointer without having first "
                   "registered it is not allowed!";
        throw SerializationException(message);
    }

    // Retrieve field information
    const DataFieldInfo& fieldInfo = fieldInfos_[fieldName];
    assert(fieldInfo.bytesPerElement() == sizeof(TData));

    // Serialize metadata and get write offset
    size_t offset = InitializeOutput(fieldInfo, savePoint);

    // Open binary file for appending the data
    std::string fname = fileBaseName(fieldInfo.name()) + ".dat";
    std::fstream fsDat(fname.c_str(),
          std::ios_base::out | std::ios_base::app | std::ios_base::binary);

    // Store offset of the beginning of the field's binary data
    fsDat.seekp(static_cast<std::streamoff>(offset), std::ios_base::beg);

    // Append binary data at the end of the file
    TData value;
    char *value_c = reinterpret_cast<char*>(&value);

    const int iStart = 0;
    const int jStart = 0;
    const int kStart = 0;
    const int lStart = 0;
    const int iEnd = fieldInfo.iSize();
    const int jEnd = fieldInfo.jSize();
    const int kEnd = fieldInfo.kSize();
    const int lEnd = fieldInfo.lSize();

    const int iSkip = 1;
    const int jSkip = iSkip * iEnd;
    const int kSkip = jSkip * jEnd;
    const int lSkip = kSkip * kEnd;

    for (int i = iStart; i < iEnd; ++i)
        for (int j = jStart; j < jEnd; ++j)
            for (int k = kStart; k < kEnd; ++k)
                for (int l = lStart; l < lEnd; ++l)
                {
                    value = *(data + i*iSkip + j*jSkip + k*kSkip + l*lSkip);
                    fsDat.write(value_c, fieldInfo.bytesPerElement());
                }

    fsDat.close();
}


#ifdef USE_STELLA
template<typename TDataField>
void Serializer::Load(TDataField& field, const SavePoint& savePoint, int instance)
{
    BOOST_STATIC_ASSERT(is_data_field<TDataField>::value);

    Serializer::Load(field, savePoint, field.name(), instance);
}

template<typename TDataField>
void Serializer::Load(TDataField& field, const SavePoint& savePoint,
                      std::string name, int instance)
{
    BOOST_STATIC_ASSERT(is_data_field<TDataField>::value);

    // define the data field value type
    typedef typename TDataField::ValueType ValueType;    
    
    // Get offset (deserialize if needed)
    size_t offset = GetInputSavePointOffset(name, savePoint, instance);
    const DataFieldInfo& fieldInfo = fieldInfos_.find(name)->second;
    const IJKSize infoDomain = fieldInfo.calculationDomain();

    // Initialize error message
    std::string error("While trying to read field ");
    error += name + " at savepoint {" + savePoint.ToString() + "} : ";
    
    // check that the value type matches the meta info
    if (fieldInfo.bytesPerElement() != sizeof(ValueType))
        throw SerializationException(error + "bytesPerElement do not match");

    // Check that definition matches
    const int fieldRank = field.storage().rank();
    if (fieldRank != fieldInfo.rank())
      error += "Ranks do not match and ";

    const IJKSize& fieldDomain = field.calculationDomain();
    if (infoDomain.iSize() != 1 && fieldDomain.iSize() != infoDomain.iSize())
        throw SerializationException(error + "I-sizes do not match");
    if (infoDomain.jSize() != 1 && fieldDomain.jSize() != infoDomain.jSize())
        throw SerializationException(error + "J-sizes do not match");
    if (infoDomain.kSize() != 1 && fieldDomain.kSize() != infoDomain.kSize())
        throw SerializationException(error + "K-sizes do not match");
    
    // Compute data length and prepare temporary buffer
    int length = fieldInfo.fieldLength();
    int numberOfValues = 1 + fieldInfo.fieldLength() / fieldInfo.bytesPerElement();
    std::vector<ValueType> values;
    values.resize(numberOfValues);

    // Open the binary file and read raw data
    std::string fname = fileBaseName(fieldInfo.name()) + ".dat";
    std::fstream fs(fname.c_str(), std::ios_base::in | std::ios_base::binary);
    fs.seekg(static_cast<std::streamoff>(offset));
    fs.read(reinterpret_cast<char*>(&values[0]), length);
    fs.close();

    // Compute indices ends
    const IJKBoundary& boundary = field.boundary();
    int iStart = std::max(-fieldInfo.iMinusHaloSize(), boundary.iMinusOffset());
    int jStart = std::max(-fieldInfo.jMinusHaloSize(), boundary.jMinusOffset());
    int kStart = std::max(-fieldInfo.kMinusHaloSize(), boundary.kMinusOffset());
    int iEnd = infoDomain.iSize()
             + std::min(fieldInfo.iPlusHaloSize(), boundary.iPlusOffset());
    int jEnd = infoDomain.jSize()
             + std::min(fieldInfo.jPlusHaloSize(), boundary.jPlusOffset());
    int kEnd = infoDomain.kSize()
             + std::min(fieldInfo.kPlusHaloSize(), boundary.kPlusOffset());

    int kSkip = 1;
    int jSkip = kSkip * (fieldInfo.kSize());
    int iSkip = jSkip * (fieldInfo.jSize());

    // Index iteration
    int index = 0;
    for (int i = iStart; i < iEnd; ++i)
        for (int j = jStart; j < jEnd; ++j)
            for (int k = kStart; k < kEnd; ++k)
            {
                index = (i+fieldInfo.iMinusHaloSize())*iSkip
                      + (j+fieldInfo.jMinusHaloSize())*jSkip
                      + (k+fieldInfo.kMinusHaloSize())*kSkip;
                field(i, j, k) = values[index];
            }

    // Store savepoint as last savepoint used
    lastSavePoint_ = savePoint;
}
#endif

template<typename TData>
void Serializer::Load(std::string fieldName, TData* data,
                      const SavePoint& savePoint, int instance)
{
    // Check whether the field was registered
    if (fieldInfos_.count(fieldName) == 0)
    {
        std::string message("Loading the field '");
        message += fieldName + "' by pointer without having first "
                   "registered it is not allowed!";
        throw SerializationException(message);
    }

    // Retrieve field information
    const DataFieldInfo& fieldInfo = fieldInfos_[fieldName];

    // Compute data length and prepare temporary buffer
    int length = fieldInfo.fieldLength();
    int numberOfValues = 1 + fieldInfo.fieldLength() / fieldInfo.bytesPerElement();
    std::vector<TData> values;
    values.resize(numberOfValues);

    // Initialize error message
    std::string error("While trying to read field ");
    error += fieldName + " at savepoint {" + savePoint.ToString() + "} : ";

    // check that the value type matches the meta info
    if (fieldInfo.bytesPerElement() != sizeof(TData))
        throw SerializationException(error + "bytesPerElement do not match");

    // Serialize metadata and get write offset
    size_t offset = GetInputSavePointOffset(fieldName, savePoint, instance);

    // Open binary file and read the data into the buffer
    std::string fname = fileBaseName(fieldInfo.name()) + ".dat";
    std::fstream fsDat(fname.c_str(), std::ios_base::in|std::ios_base::binary);
    fsDat.seekg(static_cast<std::streamoff>(offset), std::ios_base::beg);
    fsDat.read(reinterpret_cast<char*>(&values[0]), length);
    fsDat.close();

    // Compute bounds and skips
    const int iStart = 0;
    const int jStart = 0;
    const int kStart = 0;
    const int lStart = 0;
    const int iEnd = fieldInfo.iSize();
    const int jEnd = fieldInfo.jSize();
    const int kEnd = fieldInfo.kSize();
    const int lEnd = fieldInfo.lSize();

    const int iSkip = 1;
    const int jSkip = iSkip * iEnd;
    const int kSkip = jSkip * jEnd;
    const int lSkip = kSkip * kEnd;

    int idx = 0;
    for (int i = iStart; i < iEnd; ++i)
        for (int j = jStart; j < jEnd; ++j)
            for (int k = kStart; k < kEnd; ++k)
                for (int l = lStart; l < lEnd; ++l)
                {
                    *(data + i*iSkip + j*jSkip + k*kSkip + l*lSkip) = values[idx++];
                }
}

#ifdef USE_STELLA
template<typename TDataField>
void Serializer::LoadBefore(TDataField& field, const SavePoint& savePoint)
{
    BOOST_STATIC_ASSERT(is_data_field<TDataField>::value);

    Serializer::LoadBefore(field, savePoint, field.name());
}

template<typename TDataField>
void Serializer::LoadBefore(TDataField& field, const SavePoint& savePoint,
                            std::string name)
{
    BOOST_STATIC_ASSERT(is_data_field<TDataField>::value);

    DataFieldInfo fieldInfo;
    fieldInfo.Init(field);

    // Search for the preceding savepoint
    SavePoint actualSavePoint =
            indexTable_.LookupSavePointBefore(name, savePoint);

    Load(field, actualSavePoint, name);
}
#endif
