package com.aurea.testgenerator.generation.source

import com.github.javaparser.JavaParser
import com.github.javaparser.ast.ImportDeclaration
import com.github.javaparser.ast.expr.Name


class Imports {
    static final ImportDeclaration JUNIT_TEST = parse('org.junit.Test')
    static final ImportDeclaration GENERATED = parse('javax.annotation.Generated')
    static final ImportDeclaration EXCEPTION = parse('java.lang.Exception')
    static final ImportDeclaration JUNIT_RUNWITH = parse('org.junit.runner.RunWith')
    static final ImportDeclaration JUNITPARAMS_JUNITPARAMSRUNNER = parse('junitparams.JUnitParamsRunner')
    static final ImportDeclaration JUNITPARAMS_PARAMETERS = parse('junitparams.Parameters')
    static final ImportDeclaration ASSERTJ_OFFSET = parse('org.assertj.core.data.Offset')
    static final ImportDeclaration ASSERTJ_ASSERTTHAT = parseStatic('org.assertj.core.api.Assertions.assertThat')
    static final ImportDeclaration SOFT_ASSERTIONS = parse('org.assertj.core.api.SoftAssertions')
    static final ImportDeclaration COLLECTIONS = parse('java.util.Collections')
    static final ImportDeclaration IMMUTABLE_MAP = parse('com.google.common.collect.ImmutableMap')
    static final ImportDeclaration DATE = parse('java.util.Date')
    static final ImportDeclaration SQL_DATE = parse('java.sql.Date')
    static final ImportDeclaration LOCALE = parse('java.util.Locale')
    static final ImportDeclaration STATIC_MOCK = parseStatic('org.mockito.Mockito.mock')
    static final ImportDeclaration STATIC_RETURNS_DEEP_STUBS = parseStatic('org.mockito.Mockito.RETURNS_DEEP_STUBS')

    //Spring
    static final ImportDeclaration SPRING_AUTOWIRED = parse('org.springframework.beans.factory.annotation.Autowired')
    static final ImportDeclaration SPRING_TESTENTITYMANAGER = parse('org.springframework.boot.test.autoconfigure.orm.jpa.TestEntityManager')
    static final ImportDeclaration SPRING_DATAJPATEST = parse('org.springframework.boot.test.autoconfigure.orm.jpa.DataJpaTest')
    static final ImportDeclaration SPRING_SPRINGRUNNER = parse('org.springframework.test.context.junit4.SpringRunner')

    //OpenPojo
    static final ImportDeclaration OPEN_POJO_VALIDATOR = parse('com.openpojo.validation.Validator')
    static final ImportDeclaration OPEN_POJO_POJO_CLASS_FACTORY = parse('com.openpojo.reflection.impl.PojoClassFactory')
    static final ImportDeclaration OPEN_POJO_TESTERS = parse('com.aurea.unittest.commons.pojo.Testers')
    static final ImportDeclaration OPEN_POJO_GETTER_TESTER = parse('com.openpojo.validation.test.impl.GetterTester')
    static final ImportDeclaration OPEN_POJO_SETTER_TESTER = parse('com.openpojo.validation.test.impl.SetterTester')
    static final ImportDeclaration OPEN_POJO_TO_STRING_TESTER = parse('com.aurea.unittest.commons.pojo.ToStringTester')
    static final ImportDeclaration OPEN_POJO_EQUALS_VERIFIER_TESTER = parse('com.aurea.unittest.commons.pojo.equalsverifier.EqualsVerifierTesterFactory')
    static final ImportDeclaration OPEN_POJO_TEST_CHAIN = parse('com.aurea.unittest.commons.pojo.chain.TestChain')

    //PojoTester
    static final ImportDeclaration POJO_TESTER_ASSERTIONS = parse('pl.pojo.tester.api.assertion.Assertions')
    static final ImportDeclaration POJO_TESTER_ASSERTIONS_POJO_METHODS_FOR = parseStatic('pl.pojo.tester.api.assertion.Assertions.assertPojoMethodsFor')
    static final ImportDeclaration POJO_TESTER_METHOD = parse('pl.pojo.tester.api.assertion.Method')

    //Singletons
    static final ImportDeclaration SINGLETON_TESTER = parse('com.aurea.unittest.commons.SingletonTester')
    static final ImportDeclaration CALLABLE = parse('java.util.concurrent.Callable')


    static final ImportDeclaration JUNIT_BEFORE = parse('org.junit.Before')
    static final ImportDeclaration INJECT_MOCKS = parse('org.mockito.InjectMocks')
    static final ImportDeclaration MOCK = parse('org.mockito.Mock')
    static final ImportDeclaration MOCKITO = parse('org.mockito.Mockito')
    static final ImportDeclaration MOCK_ANNOTATIONS = parse('org.mockito.MockitoAnnotations')

    //spring controllers
    static final ImportDeclaration OBJECT_MAPPER = parse('com.fasterxml.jackson.databind.ObjectMapper')
    static final ImportDeclaration STATIC_MOCKITO_MATCHERS_ANY = new ImportDeclaration(new Name('org.mockito.ArgumentMatchers.any'), true, false)
    static final ImportDeclaration STATIC_MOCKITO_MATCHERS_EQ = new ImportDeclaration(new Name('org.mockito.ArgumentMatchers.eq'), true, false)
    static final ImportDeclaration STATIC_MOCKMVC_BUILDERS_POST = new ImportDeclaration(new Name('org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post'), true, false)
    static final ImportDeclaration STATIC_MOCKMVC_BUILDERS_GET = new ImportDeclaration(new Name('org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get'), true, false)
    static final ImportDeclaration STATIC_MOCKMVC_BUILDERS_DELETE = new ImportDeclaration(new Name('org.springframework.test.web.servlet.request.MockMvcRequestBuilders.delete'), true, false)
    static final ImportDeclaration STATIC_MOCKMVC_BUILDERS_PUT = new ImportDeclaration(new Name('org.springframework.test.web.servlet.request.MockMvcRequestBuilders.put'), true, false)
    static final ImportDeclaration STATIC_MOCKMVC_MATCHERS_STATUS = new ImportDeclaration(new Name('org.springframework.test.web.servlet.result.MockMvcResultMatchers.status'), true, false)

    static final ImportDeclaration MOCK_MVC = parse('org.springframework.test.web.servlet.MockMvc')
    static final ImportDeclaration MOCK_MVC_BUILDERS = parse('org.springframework.test.web.servlet.setup.MockMvcBuilders')
    static final ImportDeclaration MEDIA_TYPE = parse('org.springframework.http.MediaType')

    //trycatch
    static final ImportDeclaration STATIC_MOCKITO_ASTERISK = new ImportDeclaration(new Name('org.mockito.Mockito'), true, true)



    static ImportDeclaration parse(String fullClassName) {
        JavaParser.parseImport("import $fullClassName;")
    }

    static ImportDeclaration parseStatic(String fullClassName) {
        JavaParser.parseImport("import static $fullClassName;")
    }
}
