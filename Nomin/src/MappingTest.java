import org.gark87.yajom.example.from.Employee;
import org.gark87.yajom.example.to.Person;
import org.junit.Before;
import org.junit.Test;
import org.nomin.NominMapper;
import org.nomin.core.Nomin;

// MappingTest.java
public class MappingTest {
    NominMapper nomin = new Nomin("person2employee.groovy", "child2kid.groovy");
    Person person;
    Employee employee;

    @Before
    public void before() { /* create and initialize a person and an employee instances */ }

    @Test
    public void test() {
        Employee e = nomin.map(person, Employee.class);
        // here should be assertions to ensure that everything is ok

        Person p = nomin.map(employee, Person.class);
        // assertions again
    }
}