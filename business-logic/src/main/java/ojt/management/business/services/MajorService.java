package ojt.management.business.services;

import ojt.management.data.entities.Major;

import java.util.List;

public interface MajorService {
    Major getMajorById(Long id);

    List<Major> searchMajor(String name);

    Major updateMajor(Long id, String name);

    boolean deleteMajor(Long id);

    Major createMajor(String name);
}
