package ojt.management.business.services;

import ojt.management.common.exceptions.CompanyNotExistedException;
import ojt.management.common.exceptions.CrudException;
import ojt.management.common.payload.request.CompanyCreateRequest;
import ojt.management.common.payload.request.CompanyUpdateRequest;
import ojt.management.data.entities.Account;
import ojt.management.data.entities.Company;
import ojt.management.data.repositories.AccountRepository;
import ojt.management.data.repositories.CompanyRepository;
import ojt.management.data.repositories.RepresentativeRepository;
import org.springframework.stereotype.Service;

import java.util.List;

@Service
public class CompanyServiceImpl implements CompanyService {

    private final CompanyRepository companyRepository;
    private final AccountRepository accountRepository;
    private final RepresentativeRepository representativeRepository;

    public CompanyServiceImpl(CompanyRepository companyRepository,
                              AccountRepository accountRepository,
                              RepresentativeRepository representativeRepository) {
        this.companyRepository = companyRepository;
        this.accountRepository = accountRepository;
        this.representativeRepository = representativeRepository;
    }

    @Override
    public List<Company> searchCompany(String name, String description) {
        if (name == null && description == null) {
            return companyRepository.findAll();
        } else {
            return companyRepository.searchCompany(name, description);
        }
    }

    @Override
    public Company updateCompany(CompanyUpdateRequest companyUpdateRequest, Long accountId) throws CrudException {
        Account account = accountRepository.getById(accountId);
        if (!companyRepository.existsById(companyUpdateRequest.getId())) {
            throw new CompanyNotExistedException();
        }
        if (account.getRepresentative() != null || account.isAdmin()) {
            Company company = companyRepository.getById(companyUpdateRequest.getId());
            company.setName(companyUpdateRequest.getName());
            company.setDescription(companyUpdateRequest.getDescription());

            return companyRepository.save(company);
        }
//        return new CrudException("You can't allow access to the resource", HttpStatus.FORBIDDEN);
        return null;
    }

    @Override
    public Company getCompanyById(Long id, Long accountId) throws CrudException {
        Account account = accountRepository.getById(accountId);
        if (account.getRepresentative() != null) { //The Rep only get their company
            Long repCompanyId = representativeRepository.getById(accountId).getCompany().getId();
            if (id != repCompanyId) {
                throw new CompanyNotExistedException();
            }
        } else { //The other role can get all
            if (Boolean.FALSE.equals(companyRepository.existsById(id))) {
                throw new CompanyNotExistedException();
            }
        }
        return companyRepository.getById(id);
    }

    @Override
    public Company createCompany(CompanyCreateRequest companyCreateRequest, Long accountId) throws CrudException {
        Account account = accountRepository.getById(accountId);
        if(account.isAdmin()){
            Company company = new Company();
            company.setName(companyCreateRequest.getName());
            company.setDescription(companyCreateRequest.getDescription());
           return companyRepository.save(company);
        }
        return null;
    }
}
