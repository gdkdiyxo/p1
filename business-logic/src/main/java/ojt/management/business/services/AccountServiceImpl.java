package ojt.management.business.services;

import ojt.management.common.exceptions.AccountIdNotExistedException;
import ojt.management.common.exceptions.NotPermissionException;
import ojt.management.common.payload.request.AccountRequest;
import ojt.management.data.entities.Account;
import ojt.management.data.repositories.AccountRepository;
import ojt.management.data.repositories.MajorRepository;
import ojt.management.data.repositories.SemesterRepository;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Service;

@Service
public class AccountServiceImpl implements AccountService {

    private final AccountRepository accountRepository;
    private final MajorRepository majorRepository;
    private final SemesterRepository semesterRepository;

    public AccountServiceImpl(AccountRepository accountRepository,
                              MajorRepository majorRepository,
                              SemesterRepository semesterRepository) {
        this.accountRepository = accountRepository;
        this.majorRepository = majorRepository;
        this.semesterRepository = semesterRepository;
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
    public Account updateUser(Long id, AccountRequest accountUpdateRequest, Long accountId)
            throws AccountIdNotExistedException, NotPermissionException {
        if (Boolean.FALSE.equals(accountRepository.existsById(id))) {
            throw new AccountIdNotExistedException();
        }
        Account account = accountRepository.getById(id);
        Account accountTest = accountRepository.getById(accountId);
        // Update student
        if (accountRepository.getById(id).getStudent() != null) {
            if( (accountTest.getStudent() == null &&
                    accountTest.getRepresentative() == null) ||
                    (accountTest.getStudent() != null &&
                            account.equals(accountTest))) {
                account.setEmail(accountUpdateRequest.getEmail());
                account.setPassword(accountUpdateRequest.getPassword());
                account.setAvatar(accountUpdateRequest.getAvatar());
                account.setName(accountUpdateRequest.getName());
                account.setPhone(accountUpdateRequest.getPhone());
                account.getStudent().setAddress(accountUpdateRequest.getAddress());
                account.getStudent().setStudentCode(accountUpdateRequest.getStudentCode());
                account.getStudent().setMajor(majorRepository.getById(accountUpdateRequest.getMajorId()));
                account.getStudent().setSemester(semesterRepository.getById(accountUpdateRequest.getSemesterId()));
                accountRepository.save(account);
            } else {
                throw new NotPermissionException();
            }
        // Update Rep
        } else if(accountRepository.getById(id).getRepresentative() != null) {
            if( (accountTest.getStudent() == null &&
                    accountTest.getRepresentative() == null) ||
                    (accountTest.getRepresentative() != null &&
                            account.equals(accountTest))) {
                account.setEmail(accountUpdateRequest.getEmail());
                account.setPassword(accountUpdateRequest.getPassword());
                account.setAvatar(accountUpdateRequest.getAvatar());
                account.setName(accountUpdateRequest.getName());
                account.setPhone(accountUpdateRequest.getPhone());
                account.getRepresentative().getCompany().setName(accountUpdateRequest.getCompanyName());
                account.getRepresentative().getCompany().setAddress(accountUpdateRequest.getCompanyAddress());
                account.getRepresentative().getCompany().setDescription(accountUpdateRequest.getDescription());
                accountRepository.save(account);
            } else {
                throw new NotPermissionException();
            }
        // Update Admin
        } else {
            if (accountTest.getRepresentative() == null &&
                    accountTest.getStudent() == null){
                account.setEmail(accountUpdateRequest.getEmail());
                account.setPassword(accountUpdateRequest.getPassword());
                account.setAvatar(accountUpdateRequest.getAvatar());
                account.setName(accountUpdateRequest.getName());
                account.setPhone(accountUpdateRequest.getPhone());
                accountRepository.save(account);
            } else {
                throw new NotPermissionException();
            }
        }
        return account;
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
