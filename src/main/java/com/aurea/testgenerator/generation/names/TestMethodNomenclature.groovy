package com.aurea.testgenerator.generation.names

import com.aurea.common.ParsingUtils
import com.aurea.testgenerator.ast.ASTNodeUtils
import com.aurea.testgenerator.config.ProjectConfiguration
import com.aurea.testgenerator.generation.TestType
import com.aurea.testgenerator.generation.patterns.pojos.PojoTestTypes
import com.aurea.testgenerator.generation.patterns.springcontrollers.SpringControllersTestTypes
import com.aurea.testgenerator.generation.patterns.singleton.SingletonTypes
import com.aurea.testgenerator.generation.patterns.springrepository.SpringRepositoryTestTypes
import com.aurea.testgenerator.generation.patterns.staticfactory.StaticFactoryMethodTypes
import com.aurea.testgenerator.generation.patterns.trycatch.TryCatchTestTypes
import com.github.javaparser.ast.Node
import com.github.javaparser.ast.body.CallableDeclaration
import com.github.javaparser.ast.body.TypeDeclaration
import com.google.common.base.Splitter
import one.util.streamex.StreamEx
import pl.allegro.finance.tradukisto.ValueConverters

class TestMethodNomenclature {

    private static final String TEST_NAME_SPACE = ""
    private static final Map<? extends TestType, String> TEST_METHOD_NAME_SUFFIXES = [
            (StaticFactoryMethodTypes.IS_CALLABLE)                      : 'IsCallable',
            (StaticFactoryMethodTypes.ASSIGNMENT_CHECK)                 : 'AssignsValues',
            (StaticFactoryMethodTypes.DIFFERENT_INSTANCES)              : 'OnSecondCallCreateDifferentInstance',
            (SpringControllersTestTypes.DELEGATING)                     : 'DelegatesToService',
            (SingletonTypes.SAME_INSTANCE)                              : 'OnSecondCallReturnsSameInstance',
            (SingletonTypes.THREAD_SAFE)                                : 'IsThreadSafe',
            (SpringRepositoryTestTypes.SPRING_REPOSITORY_FIND_ENTITY)   : 'ReturnsEntity',
            (SpringRepositoryTestTypes.SPRING_REPOSITORY_FIND_MULTIPLE) : 'ReturnsList',

            (PojoTestTypes.OPEN_POJO_GETTER)                            : 'Getters',
            (PojoTestTypes.OPEN_POJO_SETTER)                            : 'Setters',
            (PojoTestTypes.OPEN_POJO_TO_STRING)                         : 'ToString',
            (PojoTestTypes.OPEN_POJO_EQUALS)                            : 'Equals',
            (PojoTestTypes.OPEN_POJO_HASH_CODE)                         : 'HashCode',
            (PojoTestTypes.OPEN_POJO_CONSTRUCTORS)                      : 'Constructors',
            (PojoTestTypes.POJO_TESTER_GETTER)                          : 'Getters',
            (PojoTestTypes.POJO_TESTER_SETTER)                          : 'Setters',
            (PojoTestTypes.POJO_TESTER_TO_STRING)                       : 'ToString',
            (PojoTestTypes.POJO_TESTER_EQUALS)                          : 'Equals',
            (PojoTestTypes.POJO_TESTER_HASH_CODE)                       : 'HashCode',
            (PojoTestTypes.POJO_TESTER_CONSTRUCTORS)                    : 'Constructors',

            (TryCatchTestTypes.RETHROW_EXCEPTION)                       : 'RethrowException',
            (TryCatchTestTypes.THROW_NEW_EXCEPTION)                     : 'ThrowNewException',
            (TryCatchTestTypes.RETURN_FROM_CATCH)                       : 'ReturnFromCatch',
            (TryCatchTestTypes.MULTIPLE_EXCEPTION)                      : 'CatchMultipleExceptions',


    ].asImmutable()

    private static final Map<? extends TestType, String> TEST_METHOD_NAME_PREFIXES = [
            (PojoTestTypes.OPEN_POJO)               : 'validate',
            (PojoTestTypes.OPEN_POJO_GETTER)        : 'validate',
            (PojoTestTypes.OPEN_POJO_SETTER)        : 'validate',
            (PojoTestTypes.OPEN_POJO_TO_STRING)     : 'validate',
            (PojoTestTypes.OPEN_POJO_EQUALS)        : 'validate',
            (PojoTestTypes.OPEN_POJO_HASH_CODE)     : 'validate',
            (PojoTestTypes.OPEN_POJO_CONSTRUCTORS)  : 'validate',
            (PojoTestTypes.POJO_TESTER_GETTER)      : 'validate',
            (PojoTestTypes.POJO_TESTER_SETTER)      : 'validate',
            (PojoTestTypes.POJO_TESTER_TO_STRING)   : 'validate',
            (PojoTestTypes.POJO_TESTER_EQUALS)      : 'validate',
            (PojoTestTypes.POJO_TESTER_HASH_CODE)   : 'validate',
            (PojoTestTypes.POJO_TESTER_CONSTRUCTORS): 'validate'
    ].asImmutable()

    final ProjectConfiguration projectConfiguration
    Set<String> takenNames = []

    TestMethodNomenclature(ProjectConfiguration projectConfiguration) {
        this.projectConfiguration = projectConfiguration
    }

    String createTestMethodName(TestType type, Node context) {
        try {
            String suffix = TEST_METHOD_NAME_SUFFIXES[type]

            switch (type) {
                case StaticFactoryMethodTypes:
                case SingletonTypes:
                case SpringControllersTestTypes:
                case SpringRepositoryTestTypes:
                case TryCatchTestTypes:
                    return new CallableNameRepository(suffix, context as CallableDeclaration).get()
                case PojoTestTypes:
                    String prefix = TEST_METHOD_NAME_PREFIXES[type]
                    return new TypeNameRepository(prefix, suffix, context as TypeDeclaration).get()

            }

            throw new IllegalArgumentException("Cannot generate name for $type in $context")
        } catch (ClassCastException cce) {
            throw new IllegalArgumentException("Cannot generate name for $type in $context", cce)
        }
    }

    String requestTestMethodName(TestType type, Node context) {
        String testName = createTestMethodName(type, context)
        takenNames << testName
        testName
    }

    private String addGlobalPrefix(String methodName) {
        projectConfiguration.methodPrefix + methodName
    }

    class CallableNameRepository {
        String suffix
        CallableDeclaration callable

        CallableNameRepository(String suffix, CallableDeclaration callable) {
            this.suffix = suffix
            this.callable = callable
        }

        String get() {
            String name = asCallableName()
            if (name in takenNames) {
                name = asNumberOfArguments()
            }
            if (name in takenNames) {
                name = asTypeOfArguments()
            }
            name
        }

        String asCallableName() {
            wrap(callable.nameAsString)
        }

        String asNumberOfArguments() {
            int numberOfParams = callable.parameters.size()
            String main = callable.nameAsString +
                    "With" +
                    integerToPascalCaseWords(numberOfParams) +
                    "Argument" + (numberOfParams > 1 ? "s" : "")
            wrap(main)
        }

        String asTypeOfArguments() {
            int numberOfParams = callable.parameters.size()
            String main = callable.nameAsString +
                    "With" + callable.parameters.collect {
                ParsingUtils.parseSimpleName(it.type.toString().replace('[]', 'Array')).capitalize()
            }.join("") + "Argument" + (numberOfParams > 1 ? "s" : "")
            wrap(main)
        }

        String wrap(String main) {
            addGlobalPrefix([main, suffix].join(TEST_NAME_SPACE))
        }
    }

    class TypeNameRepository {

        String prefix, suffix
        TypeDeclaration typeDeclaration

        TypeNameRepository(String prefix, String suffix, TypeDeclaration typeDeclaration) {
            this.prefix = prefix
            this.suffix = suffix
            this.typeDeclaration = typeDeclaration
        }

        String get() {
            String fullTypeName = ASTNodeUtils.getFullTypeName(typeDeclaration).replace('.', '')

            String name = addGlobalPrefix([prefix, fullTypeName, suffix].join(TEST_NAME_SPACE))
            if (name in takenNames) {
                name = addGlobalPrefix([prefix, fullTypeName, suffix, 'Generated'].join(TEST_NAME_SPACE))
            }
            name
        }
    }

    static String integerToPascalCaseWords(int number) {
        String withDashes = ValueConverters.ENGLISH_INTEGER.asWords(number)
        StreamEx.of(Splitter.on("-").split(withDashes).iterator())
                .map { it.capitalize() }.joining("")
    }
}

