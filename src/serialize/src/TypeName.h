// Created by Andrea Arteaga, MeteoSwiss
// Email: andyspiros@gmail.com
// January 2013

#pragma once

#include <string>

template<typename TData>
inline std::string type_name();

#define TYPENAMEFUNCTION(type) \
    template<> \
    inline std::string type_name<type>() { return #type ; }

TYPENAMEFUNCTION(int)
TYPENAMEFUNCTION(float)
TYPENAMEFUNCTION(double)
