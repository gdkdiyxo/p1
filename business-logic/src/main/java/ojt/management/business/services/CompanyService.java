package ojt.management.business.services;

import ojt.management.common.exceptions.CrudException;
import ojt.management.common.payload.request.CompanyCreateRequest;
import ojt.management.common.payload.request.CompanyUpdateRequest;
import ojt.management.data.entities.Company;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;

import java.util.List;

public interface CompanyService {
    Page<Company> searchCompany(Specification<Company> specification, Pageable pageable);

    Company updateCompany(CompanyUpdateRequest companyUpdateRequest) throws CrudException;

    Company getCompanyById(Long id, Long accountId) throws CrudException;

    Company createCompany(CompanyCreateRequest companyCreateRequest);

}
