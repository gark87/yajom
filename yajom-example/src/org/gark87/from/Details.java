package org.gark87.from;

import java.util.Date;
import java.util.List;
import java.util.Set;

public class Details {
    private Boolean sex;
    private Date birth;
    private List<Education> educations;
    private Set<Kid> kids;
    private Details details;

    public Boolean getSex() {
        return sex;
    }

    public Details getDetails() {
        return details;
    }

    public void setDetails(Details a) {
        details = a;
    }

    public void setSex(Boolean sex) {
        this.sex = sex;
    }

    public Date getBirth() {
        return birth;
    }

    public void setBirth(Date birth) {
        this.birth = birth;
    }

    public List<Education> getEducations() {
        return educations;
    }

    public void setEducations(List<Education> educations) {
        this.educations = educations;
    }

    public Set<Kid> getKids() {
        return kids;
    }

    public void setKids(Set<Kid> kids) {
        this.kids = kids;
    }
}
