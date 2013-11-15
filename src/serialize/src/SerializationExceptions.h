// Created by Andrea Arteaga, MeteoSwiss
// Email: andyspiros@gmail.com
// January 2013

#pragma once

#include <string>
#include <exception>


// Base exceptions

/**
 * This type of exceptions is raised when a problem with the serialization or
 * the deserialization is encountered.
 */
class SerializationException : public std::exception
{
public:
    SerializationException(std::string message) throw()
    : message_(message) { }

    ~SerializationException() throw() { }

    virtual const char* what() const throw() { return message_.c_str(); }
private:
    std::string message_;
};


// Specific exceptions

class SingleFileSwitch : public SerializationException
{
public:
    SingleFileSwitch()
    : SerializationException(
            "It is not allowed to change the single-file flag after the first "
            "serialization or deserialization")
    { }
};

class ParseError : public SerializationException
{
public:
    ParseError(std::string message) : SerializationException(message) { }
};


class BadFieldInfo : public SerializationException
{
public:
    BadFieldInfo(std::string message) : SerializationException(message) { }
};


class BadGlobalInfo : public SerializationException
{
public:
    BadGlobalInfo(std::string message) : SerializationException(message) { }
};


class BadMetaInfo : public SerializationException
{
public:
    BadMetaInfo(std::string message) : SerializationException(message) { }
};


class MetaInfoNotFound : public SerializationException
{
public:
    MetaInfoNotFound(std::string message) : SerializationException(message) { }
};


class FieldNotFound : public SerializationException
{
public:
    FieldNotFound(std::string message) : SerializationException(message) { }
};
