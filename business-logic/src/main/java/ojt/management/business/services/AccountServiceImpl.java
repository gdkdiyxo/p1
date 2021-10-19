package ojt.management.business.services;

import ojt.management.common.exceptions.AccountIdNotExistedException;
import ojt.management.common.payload.request.AccountCompanyUpdateRequest;
import ojt.management.common.payload.request.AccountStudentUpdateRequest;
import ojt.management.common.payload.request.AccountUpdateRequest;
import ojt.management.data.entities.Account;
import ojt.management.data.entities.Representative;
import ojt.management.data.entities.Student;
import ojt.management.data.repositories.AccountRepository;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Service;

@Service
public class AccountServiceImpl implements AccountService {

    private final AccountRepository accountRepository;

    public AccountServiceImpl(AccountRepository accountRepository) {
        this.accountRepository = accountRepository;
    }

    @Override
    public Account getUserById(Long id) throws AccountIdNotExistedException {
        if (Boolean.FALSE.equals(accountRepository.existsById(id))) {
            throw new AccountIdNotExistedException();
        } else {
            return accountRepository.getById(id);
        }
    }

    @Override
    public Page<Account> searchUser(Specification<Account> specification, Pageable pageable) {
        return accountRepository.findAll(specification, pageable);
    }

    @Override
    public Account updateUser(AccountUpdateRequest accountUpdateRequest, Long accountId) throws AccountIdNotExistedException {
        AccountStudentUpdateRequest accountStudentUpdateRequest = new AccountStudentUpdateRequest();
        AccountCompanyUpdateRequest accountCompanyUpdateRequest = new AccountCompanyUpdateRequest();

        Account account = accountRepository.getById(accountId);
        Account accountUpdated = accountRepository.getById(accountUpdateRequest.getId());

        if (Boolean.FALSE.equals(accountRepository.existsById(accountUpdateRequest.getId()))) {
            throw new AccountIdNotExistedException();
        }

        if (account.getStudent() != null) { //Khi student update
            account.setPhone(accountStudentUpdateRequest.getPhone()); //fields chung
            account.setPassword(accountStudentUpdateRequest.getPassword());
            account.getStudent().setAddress(accountStudentUpdateRequest.getAddress()); //fields riêng
        } else if (account.isAdmin()) {//Khi Ad update
            if (accountUpdated.getStudent() != null) { //Account được update là Student
                //fields chung
                account.setEmail(accountUpdateRequest.getEmail());
                account.setName(accountUpdateRequest.getName());
                account.setPassword(accountUpdateRequest.getPassword());
                account.setPhone(accountUpdateRequest.getPhone());
                //fields riêng
                account.getStudent().setAddress(accountStudentUpdateRequest.getAddress());
                account.getStudent().setStudentCode(accountStudentUpdateRequest.getStudentCode());
                account.getStudent().getMajor().setId(accountStudentUpdateRequest.getMajorId());
                account.getStudent().getSemester().setId(accountStudentUpdateRequest.getSemesterId());
            } else if (accountUpdated.getRepresentative() != null) {//Account được update là Company
                //fields chung
                account.setEmail(accountUpdateRequest.getEmail());
                account.setName(accountUpdateRequest.getName());
                account.setPassword(accountUpdateRequest.getPassword());
                account.setPhone(accountUpdateRequest.getPhone());
                //fields riêng
                account.getRepresentative().getCompany().setName(accountCompanyUpdateRequest.getCompanyName());
                account.getRepresentative().getCompany().setDescription(accountCompanyUpdateRequest.getCompanyDescription());
            }else {//Account được update là chính Admin
                account.setEmail(accountUpdateRequest.getEmail());
                account.setName(accountUpdateRequest.getName());
                account.setPassword(accountUpdateRequest.getPassword());
                account.setPhone(accountUpdateRequest.getPhone());
            }
        } else if (account.getRepresentative() != null) {//Khi Rep update
            //fields chung
            account.setPassword(accountUpdateRequest.getPassword());
            account.setPhone(accountUpdateRequest.getPhone());
            //fields riêng
            account.getRepresentative().getCompany().setDescription(accountCompanyUpdateRequest.getCompanyDescription());
        }
        return accountRepository.save(account);
    }

    @Override
    public boolean deleteUser(Long id) throws AccountIdNotExistedException {
        if (Boolean.FALSE.equals(accountRepository.existsById(id))) {
            throw new AccountIdNotExistedException();
        } else {
            Account account = accountRepository.getById(id);
            if (!account.isDisabled()) {
                account.setDisabled(true);
                return true;
            } else {
                throw new AccountIdNotExistedException();
            }
        }
    }
}
