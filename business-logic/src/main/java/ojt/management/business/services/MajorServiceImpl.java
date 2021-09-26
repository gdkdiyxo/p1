package ojt.management.business.services;

import ojt.management.data.entities.Major;
import ojt.management.data.repositories.MajorRepository;
import org.springframework.stereotype.Service;

import java.util.List;

@Service
public class MajorServiceImpl implements MajorService{

    private final MajorRepository majorRepository;

    public MajorServiceImpl(MajorRepository majorRepository) {this.majorRepository = majorRepository;}

    @Override
    public List<Major> getAllMajors () {return majorRepository.findAll();}

    @Override
    public Major getMajorById (Long id) {return majorRepository.getById(id);}

    @Override
    public Major searchMajor (String name) {return  majorRepository.searchMajor(name);}

    @Override
    public Major updateMajor (Long id, String name) {
        Major major = majorRepository.getById(id);
        major.setName(name);
        return majorRepository.getById(id);
    }

    @Override
    public boolean deleteMajor (Long id) {
        Major major = majorRepository.getById(id);
        boolean response = false;
        if (major != null) {
            major.setDisabled(true);
            response = true;
            return response;
        }
        return response;
    }

}
