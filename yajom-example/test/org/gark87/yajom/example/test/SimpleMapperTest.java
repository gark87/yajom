package org.gark87.yajom.example.test;

import junit.framework.TestCase;
import org.gark87.yajom.example.SimpleMapper;
import org.gark87.yajom.example.from.Employee;
import org.gark87.yajom.example.to.Person;

import java.util.Date;

/**
 * Unit test fpr {@link SimpleMapper}
 */
public class SimpleMapperTest extends TestCase {
    public static void testSimple() {
        Employee emp = new Employee();
        Person person = new Person();
        new SimpleMapper().map(person, emp);
    }

    public static void testFilledDetails() {
        Employee emp = new Employee();
        Person person = new Person();
        person.setBirthDate(new Date(0));
        new SimpleMapper().map(person, emp);
        assertEquals(new Date(0), emp.getDetails().getBirth());
    }
}
