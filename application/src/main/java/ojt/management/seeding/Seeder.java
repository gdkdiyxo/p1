package ojt.management.seeding;

import com.github.javafaker.Faker;
import ojt.management.business.services.EmailService;
import ojt.management.common.enums.RoleEnum;
import ojt.management.common.exceptions.*;
import ojt.management.common.payload.request.AccountRequest;
import ojt.management.controllers.AuthController;
import ojt.management.data.entities.Major;
import ojt.management.data.entities.Semester;
import ojt.management.data.repositories.AccountRepository;
import ojt.management.data.repositories.CompanyRepository;
import ojt.management.data.repositories.MajorRepository;
import ojt.management.data.repositories.SemesterRepository;
import org.springframework.boot.context.event.ApplicationReadyEvent;
import org.springframework.context.event.EventListener;
import org.springframework.stereotype.Component;
import org.thymeleaf.context.Context;
import org.thymeleaf.spring5.SpringTemplateEngine;

import java.time.ZonedDateTime;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.List;

@Component
public class Seeder {

    private final AuthController authController;
    private final AccountRepository accountRepository;
    private final MajorRepository majorRepository;
    private final CompanyRepository companyRepository;
    private final SemesterRepository semesterRepository;
    private final EmailService emailService;
    private final SpringTemplateEngine templateEngine;


    public Seeder(AuthController authController,
                  AccountRepository accountRepository,
                  MajorRepository majorRepository,
                  CompanyRepository companyRepository,
                  SemesterRepository semesterRepository,
                  EmailService emailService,
                  SpringTemplateEngine templateEngine) {
        this.authController = authController;
        this.accountRepository = accountRepository;
        this.majorRepository = majorRepository;
        this.companyRepository = companyRepository;
        this.semesterRepository = semesterRepository;
        this.emailService = emailService;
        this.templateEngine = templateEngine;
    }

    @EventListener
    private void seed(ApplicationReadyEvent event) {
        if (semesterRepository.count() == 0) {
            seedSemester();
        }
        if (majorRepository.count() == 0) {
            seedMajor();
        }
        if (accountRepository.count() == 0) {
            seedAccount();
        }
        try {
            emailService.sendMessage("anonymousvhb@gmail.com", "Something", getTemplate());
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    private void seedSemester() {
        ZonedDateTime today = ZonedDateTime.now();
        List<Semester> semesters = new ArrayList<>();
        int number = 1;
        for (int i = -2; i < 3; i++) {
            Date startDate = Date.from(today.plusMonths(i * 3).toInstant());
            Date endDate = Date.from(today.plusMonths((i + 1) * 3).toInstant());
            semesters.add(new Semester(String.format("Semester %d", number++), startDate, endDate));
        }
        semesterRepository.saveAll(semesters);
    }

    private void seedMajor() {
        List<Major> majors = Arrays.asList(
                new Major("Software Engineering"),
                new Major("Digital Art Design"),
                new Major("Information Security"),
                new Major("Information System"),
                new Major("Artificial Intelligence"),
                new Major("IoT"),
                new Major("Business Administration"),
                new Major("International Business"),
                new Major("Digital Marketing"),
                new Major("Tourism and Holiday Service Administration"),
                new Major("Multifunctional Communication Administration"),
                new Major("Hotel Management"),
                new Major("Japanese"),
                new Major("Korean")
        );
        majorRepository.saveAll(majors);
    }

    private void seedAccount() {
        Faker faker = new Faker();
        String initialPassword = "123456";
        List<AccountRequest> accountRequests = Arrays.asList(
                new AccountRequest("thanhthu0321@gmail.com", initialPassword, "Thu", RoleEnum.SYS_ADMIN.name(), faker.phoneNumber().subscriberNumber(12),null),
                new AccountRequest("student1@gmail.com", initialPassword, faker.name().fullName(), RoleEnum.STUDENT.name(), faker.phoneNumber().subscriberNumber(12), faker.address().fullAddress(), "SE150099", Long.valueOf(1), Long.valueOf(1),null),
                new AccountRequest("student2@gmail.com", initialPassword, faker.name().fullName(), RoleEnum.STUDENT.name(), faker.phoneNumber().subscriberNumber(12), faker.address().fullAddress(), "SE150098", Long.valueOf(1), Long.valueOf(1),null),
                new AccountRequest("student3@gmail.com", initialPassword, faker.name().fullName(), RoleEnum.STUDENT.name(), faker.phoneNumber().subscriberNumber(12), faker.address().fullAddress(), "BA150097", Long.valueOf(2), Long.valueOf(1),null),
                new AccountRequest("student4@gmail.com", initialPassword, faker.name().fullName(), RoleEnum.STUDENT.name(), faker.phoneNumber().subscriberNumber(12), faker.address().fullAddress(), "BA150096", Long.valueOf(2), Long.valueOf(1),null),
                new AccountRequest("student5@gmail.com", initialPassword, faker.name().fullName(), RoleEnum.STUDENT.name(), faker.phoneNumber().subscriberNumber(12), faker.address().fullAddress(), "BA150095", Long.valueOf(2), Long.valueOf(1),null),
                new AccountRequest("student6@gmail.com", initialPassword, faker.name().fullName(), RoleEnum.STUDENT.name(), faker.phoneNumber().subscriberNumber(12), faker.address().fullAddress(), "BA150094", Long.valueOf(2), Long.valueOf(1),null),
                new AccountRequest("student7@gmail.com", initialPassword, faker.name().fullName(), RoleEnum.STUDENT.name(), faker.phoneNumber().subscriberNumber(12), faker.address().fullAddress(), "BA150093", Long.valueOf(2), Long.valueOf(1),null),
                new AccountRequest("student8@gmail.com", initialPassword, faker.name().fullName(), RoleEnum.STUDENT.name(), faker.phoneNumber().subscriberNumber(12), faker.address().fullAddress(), "BA150092", Long.valueOf(2), Long.valueOf(1),null),
                new AccountRequest("student9@gmail.com", initialPassword, faker.name().fullName(), RoleEnum.STUDENT.name(), faker.phoneNumber().subscriberNumber(12), faker.address().fullAddress(), "BA150091", Long.valueOf(2), Long.valueOf(1),null),
                new AccountRequest("student10@gmail.com", initialPassword, faker.name().fullName(), RoleEnum.STUDENT.name(), faker.phoneNumber().subscriberNumber(12), faker.address().fullAddress(), "BA150090", Long.valueOf(2), Long.valueOf(1),null),
                new AccountRequest("student11@gmail.com", initialPassword, faker.name().fullName(), RoleEnum.STUDENT.name(), faker.phoneNumber().subscriberNumber(12), faker.address().fullAddress(), "BA150102", Long.valueOf(2), Long.valueOf(1),null),
                new AccountRequest("student12@gmail.com", initialPassword, faker.name().fullName(), RoleEnum.STUDENT.name(), faker.phoneNumber().subscriberNumber(12), faker.address().fullAddress(), "BA150103", Long.valueOf(2), Long.valueOf(1),null),
                new AccountRequest("student13@gmail.com", initialPassword, faker.name().fullName(), RoleEnum.STUDENT.name(), faker.phoneNumber().subscriberNumber(12), faker.address().fullAddress(), "BA150104", Long.valueOf(2), Long.valueOf(1),null),
                new AccountRequest("student14@gmail.com", initialPassword, faker.name().fullName(), RoleEnum.STUDENT.name(), faker.phoneNumber().subscriberNumber(12), faker.address().fullAddress(), "BA150105", Long.valueOf(2), Long.valueOf(1),null),
                new AccountRequest("student15@gmail.com", initialPassword, faker.name().fullName(), RoleEnum.STUDENT.name(), faker.phoneNumber().subscriberNumber(12), faker.address().fullAddress(), "BA150106", Long.valueOf(2), Long.valueOf(1),null),
                new AccountRequest("student16@gmail.com", initialPassword, faker.name().fullName(), RoleEnum.STUDENT.name(), faker.phoneNumber().subscriberNumber(12), faker.address().fullAddress(), "BA150107", Long.valueOf(2), Long.valueOf(1),null),
                new AccountRequest("student17@gmail.com", initialPassword, faker.name().fullName(), RoleEnum.STUDENT.name(), faker.phoneNumber().subscriberNumber(12), faker.address().fullAddress(), "BA150108", Long.valueOf(2), Long.valueOf(1),null),
                new AccountRequest("student18@gmail.com", initialPassword, faker.name().fullName(), RoleEnum.STUDENT.name(), faker.phoneNumber().subscriberNumber(12), faker.address().fullAddress(), "BA150109", Long.valueOf(2), Long.valueOf(1),null),
                new AccountRequest("student19@gmail.com", initialPassword, faker.name().fullName(), RoleEnum.STUDENT.name(), faker.phoneNumber().subscriberNumber(12), faker.address().fullAddress(), "BA150110", Long.valueOf(2), Long.valueOf(1),null),
                new AccountRequest("student20@gmail.com", initialPassword, faker.name().fullName(), RoleEnum.STUDENT.name(), faker.phoneNumber().subscriberNumber(12), faker.address().fullAddress(), "BA150111", Long.valueOf(2), Long.valueOf(1),null),
                new AccountRequest("student21@gmail.com", initialPassword, faker.name().fullName(), RoleEnum.STUDENT.name(), faker.phoneNumber().subscriberNumber(12), faker.address().fullAddress(), "BA150112", Long.valueOf(2), Long.valueOf(1),null),
                new AccountRequest("student22@gmail.com", initialPassword, faker.name().fullName(), RoleEnum.STUDENT.name(), faker.phoneNumber().subscriberNumber(12), faker.address().fullAddress(), "BA150113", Long.valueOf(2), Long.valueOf(1),null),
                new AccountRequest("student23@gmail.com", initialPassword, faker.name().fullName(), RoleEnum.STUDENT.name(), faker.phoneNumber().subscriberNumber(12), faker.address().fullAddress(), "BA150114", Long.valueOf(2), Long.valueOf(1),null),
                new AccountRequest("student24@gmail.com", initialPassword, faker.name().fullName(), RoleEnum.STUDENT.name(), faker.phoneNumber().subscriberNumber(12), faker.address().fullAddress(), "BA150115", Long.valueOf(2), Long.valueOf(1),null),
                new AccountRequest("student25@gmail.com", initialPassword, faker.name().fullName(), RoleEnum.STUDENT.name(), faker.phoneNumber().subscriberNumber(12), faker.address().fullAddress(), "BA150116", Long.valueOf(2), Long.valueOf(1),null),
                new AccountRequest("student26@gmail.com", initialPassword, faker.name().fullName(), RoleEnum.STUDENT.name(), faker.phoneNumber().subscriberNumber(12), faker.address().fullAddress(), "BA150117", Long.valueOf(2), Long.valueOf(1),null),
                new AccountRequest("student27@gmail.com", initialPassword, faker.name().fullName(), RoleEnum.STUDENT.name(), faker.phoneNumber().subscriberNumber(12), faker.address().fullAddress(), "BA150118", Long.valueOf(2), Long.valueOf(1),null),
                new AccountRequest("student28@gmail.com", initialPassword, faker.name().fullName(), RoleEnum.STUDENT.name(), faker.phoneNumber().subscriberNumber(12), faker.address().fullAddress(), "BA150119", Long.valueOf(2), Long.valueOf(1),null),
                new AccountRequest("student29@gmail.com", initialPassword, faker.name().fullName(), RoleEnum.STUDENT.name(), faker.phoneNumber().subscriberNumber(12), faker.address().fullAddress(), "BA150120", Long.valueOf(2), Long.valueOf(1),null),
                new AccountRequest("student30@gmail.com", initialPassword, faker.name().fullName(), RoleEnum.STUDENT.name(), faker.phoneNumber().subscriberNumber(12), faker.address().fullAddress(), "BA150121", Long.valueOf(2), Long.valueOf(1),null),
                new AccountRequest("student31@gmail.com", initialPassword, faker.name().fullName(), RoleEnum.STUDENT.name(), faker.phoneNumber().subscriberNumber(12), faker.address().fullAddress(), "BA150122", Long.valueOf(2), Long.valueOf(1),null),
                new AccountRequest("student32@gmail.com", initialPassword, faker.name().fullName(), RoleEnum.STUDENT.name(), faker.phoneNumber().subscriberNumber(12), faker.address().fullAddress(), "BA150123", Long.valueOf(2), Long.valueOf(1),null),
                new AccountRequest("student33@gmail.com", initialPassword, faker.name().fullName(), RoleEnum.STUDENT.name(), faker.phoneNumber().subscriberNumber(12), faker.address().fullAddress(), "SE150123", Long.valueOf(2), Long.valueOf(1),null),
                new AccountRequest("student34@gmail.com", initialPassword, faker.name().fullName(), RoleEnum.STUDENT.name(), faker.phoneNumber().subscriberNumber(12), faker.address().fullAddress(), "SE150124", Long.valueOf(1), Long.valueOf(1),null),
                new AccountRequest("student35@gmail.com", initialPassword, faker.name().fullName(), RoleEnum.STUDENT.name(), faker.phoneNumber().subscriberNumber(12), faker.address().fullAddress(), "SE150125", Long.valueOf(1), Long.valueOf(1),null),
                new AccountRequest("student36@gmail.com", initialPassword, faker.name().fullName(), RoleEnum.STUDENT.name(), faker.phoneNumber().subscriberNumber(12), faker.address().fullAddress(), "SE150126", Long.valueOf(1), Long.valueOf(1),null),
                new AccountRequest("student37@gmail.com", initialPassword, faker.name().fullName(), RoleEnum.STUDENT.name(), faker.phoneNumber().subscriberNumber(12), faker.address().fullAddress(), "SE150127", Long.valueOf(1), Long.valueOf(1),null),
                new AccountRequest("student38@gmail.com", initialPassword, faker.name().fullName(), RoleEnum.STUDENT.name(), faker.phoneNumber().subscriberNumber(12), faker.address().fullAddress(), "SE150128", Long.valueOf(1), Long.valueOf(1),null),
                new AccountRequest("student39@gmail.com", initialPassword, faker.name().fullName(), RoleEnum.STUDENT.name(), faker.phoneNumber().subscriberNumber(12), faker.address().fullAddress(), "SE150129", Long.valueOf(1), Long.valueOf(1),null),
                new AccountRequest("student40@gmail.com", initialPassword, faker.name().fullName(), RoleEnum.STUDENT.name(), faker.phoneNumber().subscriberNumber(12), faker.address().fullAddress(), "SE150130", Long.valueOf(1), Long.valueOf(1),null),
                new AccountRequest("student41@gmail.com", initialPassword, faker.name().fullName(), RoleEnum.STUDENT.name(), faker.phoneNumber().subscriberNumber(12), faker.address().fullAddress(), "SE150131", Long.valueOf(1), Long.valueOf(1),null),
                new AccountRequest("student42@gmail.com", initialPassword, faker.name().fullName(), RoleEnum.STUDENT.name(), faker.phoneNumber().subscriberNumber(12), faker.address().fullAddress(), "SE150132", Long.valueOf(1), Long.valueOf(1),null),
                new AccountRequest("student43@gmail.com", initialPassword, faker.name().fullName(), RoleEnum.STUDENT.name(), faker.phoneNumber().subscriberNumber(12), faker.address().fullAddress(), "SE150133", Long.valueOf(1), Long.valueOf(1),null),
                new AccountRequest("student44@gmail.com", initialPassword, faker.name().fullName(), RoleEnum.STUDENT.name(), faker.phoneNumber().subscriberNumber(12), faker.address().fullAddress(), "SE150134", Long.valueOf(1), Long.valueOf(1),null),
                new AccountRequest("student45@gmail.com", initialPassword, faker.name().fullName(), RoleEnum.STUDENT.name(), faker.phoneNumber().subscriberNumber(12), faker.address().fullAddress(), "SE150135", Long.valueOf(1), Long.valueOf(1),null),
                new AccountRequest("student46@gmail.com", initialPassword, faker.name().fullName(), RoleEnum.STUDENT.name(), faker.phoneNumber().subscriberNumber(12), faker.address().fullAddress(), "SE150136", Long.valueOf(1), Long.valueOf(1),null),
                new AccountRequest("student47@gmail.com", initialPassword, faker.name().fullName(), RoleEnum.STUDENT.name(), faker.phoneNumber().subscriberNumber(12), faker.address().fullAddress(), "SE150137", Long.valueOf(1), Long.valueOf(1),null),
                new AccountRequest("student48@gmail.com", initialPassword, faker.name().fullName(), RoleEnum.STUDENT.name(), faker.phoneNumber().subscriberNumber(12), faker.address().fullAddress(), "SE150138", Long.valueOf(1), Long.valueOf(1),null),
                new AccountRequest("student49@gmail.com", initialPassword, faker.name().fullName(), RoleEnum.STUDENT.name(), faker.phoneNumber().subscriberNumber(12), faker.address().fullAddress(), "SE150139", Long.valueOf(1), Long.valueOf(1),null),
                new AccountRequest("student50@gmail.com", initialPassword, faker.name().fullName(), RoleEnum.STUDENT.name(), faker.phoneNumber().subscriberNumber(12), faker.address().fullAddress(), "SE150140", Long.valueOf(1), Long.valueOf(1),null),
                new AccountRequest("representative1@gmail.com", initialPassword, faker.name().fullName(), RoleEnum.COMPANY_REPRESENTATIVE.name(), faker.company().name(), faker.educator().university(), faker.address().fullAddress(), faker.phoneNumber().subscriberNumber(12), null),
                new AccountRequest("representative2@gmail.com", initialPassword, faker.name().fullName(), RoleEnum.COMPANY_REPRESENTATIVE.name(), faker.company().name(), faker.educator().university(), faker.address().fullAddress(), faker.phoneNumber().subscriberNumber(12), null),
                new AccountRequest("representative3@gmail.com", initialPassword, faker.name().fullName(), RoleEnum.COMPANY_REPRESENTATIVE.name(), faker.company().name(), faker.educator().university(), faker.address().fullAddress(), faker.phoneNumber().subscriberNumber(12), null),
                new AccountRequest("representative4@gmail.com", initialPassword, faker.name().fullName(), RoleEnum.COMPANY_REPRESENTATIVE.name(), faker.company().name(), faker.educator().university(), faker.address().fullAddress(), faker.phoneNumber().subscriberNumber(12), null),
                new AccountRequest("representative5@gmail.com", initialPassword, faker.name().fullName(), RoleEnum.COMPANY_REPRESENTATIVE.name(), faker.company().name(), faker.educator().university(), faker.address().fullAddress(), faker.phoneNumber().subscriberNumber(12), null),
                new AccountRequest("representative6@gmail.com", initialPassword, faker.name().fullName(), RoleEnum.COMPANY_REPRESENTATIVE.name(), faker.company().name(), faker.educator().university(), faker.address().fullAddress(), faker.phoneNumber().subscriberNumber(12), null),
                new AccountRequest("representative7@gmail.com", initialPassword, faker.name().fullName(), RoleEnum.COMPANY_REPRESENTATIVE.name(), faker.company().name(), faker.educator().university(), faker.address().fullAddress(), faker.phoneNumber().subscriberNumber(12), null),
                new AccountRequest("representative8@gmail.com", initialPassword, faker.name().fullName(), RoleEnum.COMPANY_REPRESENTATIVE.name(), faker.company().name(), faker.educator().university(), faker.address().fullAddress(), faker.phoneNumber().subscriberNumber(12), null));

        accountRequests.stream().forEach(accountRequest -> {
            try {
                authController.registerUser(accountRequest);
            } catch (UsernameAlreadyExistedException |
                    EmailAlreadyExistedException |
                    EmptyRoleException |
                    CompanyNotExistedException |
                    MajorNotExistedException |
                    SemesterNotExistedException e) {
                e.printStackTrace();
            }
        });
    }

    private String getTemplate() {
        Context context = new Context();
        return templateEngine.process("welcome.html", context);
    }
}
