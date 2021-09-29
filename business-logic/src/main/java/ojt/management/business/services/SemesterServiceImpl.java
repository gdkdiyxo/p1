package ojt.management.business.services;

import ojt.management.data.entities.Semester;
import ojt.management.data.repositories.SemesterRepository;
import org.springframework.stereotype.Service;

import java.util.Date;
import java.util.List;

@Service
public class SemesterServiceImpl implements SemesterService{

    private final SemesterRepository semesterRepository;

    public SemesterServiceImpl(SemesterRepository semesterRepository) { this.semesterRepository = semesterRepository;}

    @Override
    public Semester getById(Long id) {
        return semesterRepository.getById(id);
    }

    @Override
    public List<Semester> searchSemesters(String name, Date startDate, Date endDate) {
        if (name == null & startDate == null & endDate == null) {
            return semesterRepository.findAll();
        }
        return  semesterRepository.searchSemester(name, startDate, endDate);
    }

    @Override
    public Semester updateSemester(Long id, String name, Date startDate, Date endDate) {
        Semester semester = semesterRepository.getById(id);
        if (name != null) { semester.setName(name); }
        if (startDate != null) { semester.setStartDate(startDate); }
        if (endDate != null) { semester.setEndDate(endDate); }
        semesterRepository.save(semester);
        return semesterRepository.getById(id);
    }

    @Override
    public boolean deleteSemester(Long id) {
        Semester semester = semesterRepository.getById(id);
        boolean response = false;
        if (semester != null || semester.isDisabled()==false) {
            semester.setDisabled(true);
            response = true;
            return response;
        }
        return response;
    }

    @Override
    public Semester createSemester(String name, Date startDate, Date endDate) {
        Semester semester = new Semester(name, startDate, endDate);
        semesterRepository.save(semester);
        return semesterRepository.getById(semester.getId());

    }
}
