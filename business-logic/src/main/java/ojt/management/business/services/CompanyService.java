package ojt.management.business.services;

import ojt.management.common.exceptions.CrudException;
import ojt.management.common.payload.request.CompanyRequest;
import ojt.management.data.entities.Company;

import java.util.List;

public interface CompanyService {
    List<Company> searchCompany(String name, String description);

    Company updateCompany(Long id, CompanyRequest companyRequest) throws CrudException;

    Company getCompanyById(Long id, Long accountId) throws CrudException;

    Company createCompany(CompanyRequest companyRequest);

}
