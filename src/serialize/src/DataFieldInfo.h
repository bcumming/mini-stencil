// Created by Andrea Arteaga, MeteoSwiss
// Email: andyspiros@gmail.com
// January 2013

#pragma once

#include <stdexcept>
#include <string>
#include <sstream>
#include <vector>
#include <algorithm>

class JSONNode;

class DataFieldInfo
{
public:
    /**
     * Default constructor
     */
    DataFieldInfo()
    {
        name_ = "";
        type_ = "";
        bytesPerElement_ = 0;
        rank_ = 0;
        iSize_ = 1;
        jSize_ = 1;
        kSize_ = 1;
        lSize_ = 1;
        iPlusHalo_ = 0;
        jPlusHalo_ = 0;
        kPlusHalo_ = 0;
        lPlusHalo_ = 0;
        iMinusHalo_ = 0;
        jMinusHalo_ = 0;
        kMinusHalo_ = 0;
        lMinusHalo_ = 0;
    }

    /**
     * Copy constructor
     */
    DataFieldInfo(const DataFieldInfo& other)
    {
        *this = other;
    }

    /**
     * Assignment operator
     */
    DataFieldInfo& operator=(const DataFieldInfo& other)
    {
        name_ = other.name_;
        type_ = other.type_;
        bytesPerElement_ = other.bytesPerElement_;
        rank_ = other.rank_;
        iSize_ = other.iSize_;
        jSize_ = other.jSize_;
        kSize_ = other.kSize_;
        lSize_ = other.lSize_;
        iPlusHalo_ = other.iPlusHalo_;
        jPlusHalo_ = other.jPlusHalo_;
        kPlusHalo_ = other.kPlusHalo_;
        lPlusHalo_ = other.lPlusHalo_;
        iMinusHalo_ = other.iMinusHalo_;
        jMinusHalo_ = other.jMinusHalo_;
        kMinusHalo_ = other.kMinusHalo_;
        lMinusHalo_ = other.lMinusHalo_;
        metainfo_ = other.metainfo_;

        return *this;
    }


    /**
     * Initializer with field.
     *
     * This general-purpose initializer recovers the information
     * from a given data field.
     *
     * @param dataField The field of which the information must be read
     */
    template<typename TDataField>
    void Init(const TDataField& dataField)
    {
        Init(dataField, dataField.name());
    }


#ifdef USE_STELLA
    /**
     * Initializer with field and new name.
     *
     * This general-purpose initializer recovers the information
     * from a given data field.
     *
     * @param dataField The field of which the information must be read
     * @param name The name of the field when serialized
     */
    template<typename TDataField>
    void Init(const TDataField& dataField, std::string name)
    {
        // Get the calculation domain
        const IJKSize& domain = dataField.calculationDomain();
        const IJKSize& size = dataField.storage().allocatedSize();

        // Get the size of the halo
        const IJKBoundary& boundary = dataField.boundary();

        // Workaround for boundary() not returning what it should
        const bool haloInI = !(size.iSize() <= domain.iSize());
        const bool haloInJ = !(size.jSize() <= domain.jSize());
        const bool haloInK = !(size.kSize() <= domain.kSize());
        const int iMinus = haloInI ? -boundary.iMinusOffset() : 0;
        const int iPlus = haloInI ? boundary.iPlusOffset() : 0;
        const int jMinus = haloInJ ? -boundary.jMinusOffset() : 0;
        const int jPlus = haloInJ ? boundary.jPlusOffset() : 0;
        const int kMinus = haloInK ? -boundary.kMinusOffset() : 0;
        const int kPlus = haloInK ? boundary.kPlusOffset() : 0;

        // Delegate the initialization
        Init(
            name,
            type_name<typename TDataField::ValueType>(),
            sizeof(typename TDataField::ValueType),
            dataField.storage().rank(),
            size.iSize(),
            size.jSize(),
            size.kSize(),
            1,
            iMinus,
            iPlus,
            jMinus,
            jPlus,
            kMinus,
            kPlus,
            0,
            0
          );
    }
#endif


    /**
     * Initializer with sparse information.
     *
     * This initializer collects the information given in the multiple
     * parameters.
     *
     * @param name The name of the field
     * @param type The data type of the field (e.g. "double")
     * @param bytesPerElement The size in bytes of a single entry
     * @param rank The number of dimensions of the field
     * @param iSize The size in i-direction (including of halo)
     * @param jSize The size in j-direction (including of halo)
     * @param kSize The size in k-direction (including of halo)
     * @param lSize The size in l-direction (including of halo)
     * @param iMinusHalo The dimension of the halo in negative i-direction
     * @param iPlusHalo  The dimension of the halo in positive i-direction
     * @param jMinusHalo The dimension of the halo in negative j-direction
     * @param jPlusHalo  The dimension of the halo in positive j-direction
     * @param kMinusHalo The dimension of the halo in negative k-direction
     * @param kPlusHalo  The dimension of the halo in positive k-direction
     * @param lMinusHalo The dimension of the halo in negative l-direction
     * @param lPlusHalo  The dimension of the halo in positive l-direction
     */
    void Init(std::string name, std::string type, int bytesPerElement, int rank,
            int iSize, int jSize, int kSize, int lSize,
            int iMinusHalo, int iPlusHalo, int jMinusHalo, int jPlusHalo,
            int kMinusHalo, int kPlusHalo, int lMinusHalo, int lPlusHalo
        )
    {
        name_ = name;
        type_ = type;
        bytesPerElement_ = bytesPerElement;
        rank_ = rank;

        iSize_ = iSize;
        jSize_ = jSize;
        kSize_ = kSize;
        lSize_ = lSize;

        iMinusHalo_ = iMinusHalo;
        iPlusHalo_  = iPlusHalo;
        jMinusHalo_ = jMinusHalo;
        jPlusHalo_  = jPlusHalo;
        kMinusHalo_ = kMinusHalo;
        kPlusHalo_  = kPlusHalo;
        lMinusHalo_ = lMinusHalo;
        lPlusHalo_  = lPlusHalo;
    }

    JSONNode Serialize(int id=-1) const;
    void Deserialize(const JSONNode& fieldNode);

    /**
     * @returns The metainformation set associated with the field
     */
    const std::vector<MetaInfo>& metainfo() const
    {
        return metainfo_;
    }

    /**
     * @returns The field name
     */
    const std::string& name() const
    {
        return name_;
    }

    /**
     * Sets the name to a value different than that of the initialization field
     *
     * @param name The new name
     */
    void set_Name(std::string name)
    {
        if(name.empty())
        {
            throw std::invalid_argument("Passed void name to DataFieldInfo");
        }
        name_ = name;
    }

    /**
     * @returns The field scalar type
     */
    const std::string& type() const
    {
        return type_;
    }

    /**
     * @returns The dimension in bytes of a single element
     */
    int bytesPerElement() const
    {
        return bytesPerElement_;
    }

    /**
     * @returns The number of dimensions of the field
     */
    int rank() const
    {
        return rank_;
    }

    /**
     * @returns The size of the field in i-direction, including the halo
     */
    int iSize() const
    {
        return iSize_;
    }

    /**
     * @returns The size of the field in j-direction, including the halo
     */
    int jSize() const
    {
        return jSize_;
    }

    /**
     * @returns The size of the field in k-direction, including the halo
     */
    int kSize() const
    {
        return kSize_;
    }

    /**
     * @returns The size of the field in l-direction, including the halo
     */
    int lSize() const
    {
        return lSize_;
    }


    /**
     * @returns The size of the halo in negative i-direction
     */
    int iMinusHaloSize() const
    {
        return iMinusHalo_;
    }

    /**
     * @returns The size of the halo in positive i-direction
     */
    int iPlusHaloSize() const
    {
        return iPlusHalo_;
    }

    /**
     * @returns The size of the halo in negative i-direction
     */
    int jMinusHaloSize() const
    {
        return jMinusHalo_;
    }

    /**
     * @returns The size of the halo in positive i-direction
     */
    int jPlusHaloSize() const
    {
        return jPlusHalo_;
    }

    /**
     * @returns The size of the halo in negative i-direction
     */
    int kMinusHaloSize() const
    {
        return kMinusHalo_;
    }

    /**
     * @returns The size of the halo in positive i-direction
     */
    int kPlusHaloSize() const
    {
        return kPlusHalo_;
    }

    /**
     * @returns The size of the halo in negative i-direction
     */
    int lMinusHaloSize() const
    {
        return lMinusHalo_;
    }

    /**
     * @returns The size of the halo in positive i-direction
     */
    int lPlusHaloSize() const
    {
        return lPlusHalo_;
    }

    /**
     * @returns The number of bytes occupied by the field
     */
    int fieldLength() const
    {
        return bytesPerElement_ * iSize_ * jSize_ * kSize_ * lSize_;
    }


#ifdef USE_STELLA
    /**
     * @returns The calculation domain of the field
     */
    IJKSize calculationDomain() const
    {
        IJKSize size;
        size.Init(
                iSize_ - iMinusHalo_ - iPlusHalo_,
                jSize_ - jMinusHalo_ - jPlusHalo_,
                kSize_ - kMinusHalo_ - kPlusHalo_
            );
        return size;
    }


    /**
     * @returns The total storage of the field (calculation domain and boundary)
     */
    IJKSize size() const
    {
        IJKSize size;
        size.Init(iSize_, jSize_, kSize_);
        return size;
    }

    /**
     * @returns The size of the halo of the field
     */
    IJKBoundary boundary() const
    {
        IJKBoundary boundary;
        boundary.Init(-iMinusHalo_, iPlusHalo_,
                      -jMinusHalo_, jPlusHalo_,
                      -kMinusHalo_, kPlusHalo_);
        return boundary;
    }
#endif

    /**
     * Add new metainformation
     *
     * @param info An initialized MetaInfo object
     */
    void AddMetaInfo(const MetaInfo& info)
    {
        metainfo_.push_back(info);
    }

    /**
     * Comparator operator
     *
     * \returns true if and only if the parameter contains exactly the same
     *          information as the current object.
     */
    bool operator==(const DataFieldInfo& other) const
    {
        // Compare the least expensive elements first
        bool value = (
                bytesPerElement_ == other.bytesPerElement_ &&
                rank_ == other.rank_ &&

                iSize_ == other.iSize_ &&
                jSize_ == other.jSize_ &&
                kSize_ == other.kSize_ &&
                lSize_ == other.lSize_ &&

                iMinusHalo_ == other.iMinusHalo_ &&
                iPlusHalo_  == other.iPlusHalo_ &&
                jMinusHalo_ == other.jMinusHalo_ &&
                jPlusHalo_  == other.jPlusHalo_ &&
                kMinusHalo_ == other.kMinusHalo_ &&
                kPlusHalo_  == other.kPlusHalo_ &&
                lMinusHalo_ == other.lMinusHalo_ &&
                lPlusHalo_  == other.lPlusHalo_ &&

                name_ == other.name_ &&
                type_ == other.type_
            );

        if (!value)
            return false;

        // Compare metadata (order is not important)
        const std::vector<MetaInfo>& othermeta = other.metainfo_;
        if (metainfo_.size() != other.metainfo_.size())
            return false;

        typedef std::vector<MetaInfo>::const_iterator iter_t;
        for (iter_t iter = metainfo_.begin(); iter != metainfo_.end(); ++iter)
        {
            if (
                std::find(othermeta.begin(), othermeta.end(), *iter)
                    == othermeta.end()
                )
                return false;
        }

        return true;
    }

    /**
     * Comparator operator
     *
     * \returns false if and only if the parameter contains exactly the same
     *          information as the current object.
     */
    bool operator!=(const DataFieldInfo& other) const
    {
        return !(*this == other);
    }

    std::string ToString() const
    {
        std::ostringstream strm;
        strm << name_ << " (" 
             << iSize_ << "x" << jSize_ << "x" << kSize_ << "x" << lSize_
             << ")";
        return strm.str();
    }



private:
    std::string name_;
    std::string type_;
    int bytesPerElement_;

    int rank_;

    // Sizes: contain the total size (caculation domain and boundaries)
    int iSize_;
    int jSize_;
    int kSize_;
    int lSize_;

    int iPlusHalo_, iMinusHalo_;
    int jPlusHalo_, jMinusHalo_;
    int kPlusHalo_, kMinusHalo_;
    int lPlusHalo_, lMinusHalo_;

    std::vector<MetaInfo> metainfo_;
};



