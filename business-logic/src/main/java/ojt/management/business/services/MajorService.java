package ojt.management.business.services;

import ojt.management.data.entities.Major;

import java.util.List;

public interface MajorService {
    List<Major> getAllMajors();

    Major getMajorById(Long id);

    Major searchMajor(String name);

    Major updateMajor(Long id, String name);

    boolean deleteMajor(Long id);
}
