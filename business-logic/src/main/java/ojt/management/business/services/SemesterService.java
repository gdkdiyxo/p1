package ojt.management.business.services;

import ojt.management.data.entities.Semester;

import java.util.Date;
import java.util.List;

public interface SemesterService {
    Semester getById(Long id);

    List<Semester> searchSemesters(String name, Date startDate, Date endDate);

    Semester updateSemester(Long id, String name, Date startDate, Date endDate);

    boolean deleteSemester(Long id);
    // test comment
}
