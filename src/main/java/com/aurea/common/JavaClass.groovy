package com.aurea.common

import groovy.transform.EqualsAndHashCode
import groovy.transform.ToString

@EqualsAndHashCode
@ToString(includePackage = false)
class JavaClass {
    String fullName

    JavaClass(String fullName) {
        this.fullName = fullName
    }

    JavaClass(String packageName, String className) {
        this(ParsingUtils.createFullName(packageName, className))
    }

    String getPackage() {
        ParsingUtils.parsePackage(fullName)
    }

    String getName() {
        ParsingUtils.parseSimpleName(fullName)
    }
}