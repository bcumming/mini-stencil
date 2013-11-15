// Created by Andrea Arteaga, MeteoSwiss
// Email: andyspiros@gmail.com
// January 2013

#pragma once

// Inclusion for dependency
#ifdef USE_STELLA
    #include "Definitions.h"
    #include "SharedInfrastructure.h"
#else
    #include "Util.h"
#endif
#include <boost/mpl/bool.hpp>
#include <boost/mpl/if.hpp>

// Internal includes
#include "SerializationExceptions.h"
#include "TypeName.h"
#include "MetaInfo.h"
#include "DataFieldInfo.h"
#include "SavePoint.h"
#include "IndexTableEntry.h"
#include "IndexTable.h"
#include "SerializerInput.h"
#include "SerializerOutput.h"
#include "MetaDataSerializer.h"
#include "Serializer.h"

// Implementation of inline and template methods
// Loaded at the end to avoid circular dependencies
#include "SerializerOutput.impl.h"
#include "SerializerInput.impl.h"


// Add the distributed serializer, if we have GCL
#ifdef __GCL__
    #include "CommunicationFramework.h"
    #include "DistributedSerializer.h"
#endif
