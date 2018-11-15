package com.aurea.testgenerator.generation.patterns.trycatch;

import com.aurea.testgenerator.generation.TestType;

public enum TryCatchTestTypes implements TestType {
    RETHROW_EXCEPTION,
    THROW_NEW_EXCEPTION,
    MULTIPLE_EXCEPTION,
    RETURN_FROM_CATCH
}
