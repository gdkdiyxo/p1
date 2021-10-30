package ojt.management.business.services;

import ojt.management.common.exceptions.AccountIdNotExistedException;
import ojt.management.common.exceptions.EvaluationIdNotExistedException;
import ojt.management.common.payload.request.EvaluationCreateRequest;
import ojt.management.common.payload.request.EvaluationUpdateRequest;
import ojt.management.data.entities.Evaluation;
import ojt.management.data.repositories.AccountRepository;
import ojt.management.data.repositories.EvaluationRepository;
import org.springframework.stereotype.Service;

import java.util.List;

@Service
public class EvaluationServiceImpl implements EvaluationService{

    private final EvaluationRepository evaluationRepository;
    private final AccountRepository accountRepository;

    public EvaluationServiceImpl (EvaluationRepository evaluationRepository,
                                  AccountRepository accountRepository){
        this.evaluationRepository = evaluationRepository;
        this.accountRepository = accountRepository;
    }

    @Override
    public List<Evaluation> searchEvaluation(Long accountId)
            throws AccountIdNotExistedException {
        if (Boolean.FALSE.equals(accountRepository.existsById(accountId))){
            throw new AccountIdNotExistedException();
        }
        if (accountRepository.getById(accountId).getRepresentative() != null){
            return evaluationRepository.searchEvaluationRep(accountRepository.getById(accountId).getRepresentative().getCompany().getId());
        } else{
            return evaluationRepository.searchEvaluationStudent(accountRepository.getById(accountId).getStudent().getId());
        }
    }

    @Override
    public Evaluation getEvaluationById(Long id, Long accountId)
            throws EvaluationIdNotExistedException, AccountIdNotExistedException {
        if (Boolean.FALSE.equals(evaluationRepository.existsById(id))){
            throw new EvaluationIdNotExistedException();
        }
        if (Boolean.FALSE.equals(accountRepository.existsById(accountId))){
            throw new AccountIdNotExistedException();
        }
        if (accountRepository.getById(accountId).getRepresentative() != null){
            return evaluationRepository.getEvaluationRep(accountRepository.getById(accountId).getRepresentative().getCompany().getId(), id);
        } else{
            return evaluationRepository.getEvaluationStudent(accountRepository.getById(accountId).getStudent().getId(), id);
        }
    }

    @Override
    public Evaluation updateEvaluation(Long id, EvaluationUpdateRequest evaluationUpdateRequest, Long accountId)
            throws EvaluationIdNotExistedException, AccountIdNotExistedException {
        if (Boolean.FALSE.equals(evaluationRepository.existsById(id))){
            throw new EvaluationIdNotExistedException();
        }
        if (Boolean.FALSE.equals(accountRepository.existsById(accountId))){
            throw new AccountIdNotExistedException();
        }
        Evaluation evaluation = evaluationRepository.getEvaluationRep(accountRepository.getById(accountId).getRepresentative().getCompany().getId(), id);
        evaluation.setComment(evaluationUpdateRequest.getComment());
        evaluation.setPass(evaluationUpdateRequest.isPass());
        evaluationRepository.save(evaluation);
        return evaluation;
    }

    @Override
    public Evaluation createEvaluation(EvaluationCreateRequest evaluationCreateRequest, Long accountId){
        Evaluation evaluation = new Evaluation();
        evaluation.setGrade(evaluationCreateRequest.getGrade());
        evaluation.setComment(evaluationCreateRequest.getComment());
        evaluation.setApplication(evaluation.getApplication());
        evaluationRepository.save(evaluation);
        return evaluation;
    }
}
