package ojt.management.business.services;

import ojt.management.data.entities.Company;

import java.util.List;

public interface CompanyService {
    List<Company> searchCompany(String name, String description);

    Company updateCompany(String name, String description);
}
