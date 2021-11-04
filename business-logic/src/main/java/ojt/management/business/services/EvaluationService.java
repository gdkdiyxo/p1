package ojt.management.business.services;

import ojt.management.common.exceptions.AccountIdNotExistedException;
import ojt.management.common.exceptions.EvaluationIdNotExistedException;
import ojt.management.common.payload.request.EvaluationCreateRequest;
import ojt.management.common.payload.request.EvaluationUpdateRequest;
import ojt.management.data.entities.Evaluation;

import java.util.List;

public interface EvaluationService {
    Evaluation getEvaluationById(Long id, Long accountId)
            throws EvaluationIdNotExistedException, AccountIdNotExistedException;

    List<Evaluation> searchEvaluation(String studentCode, Long accountId)
            throws AccountIdNotExistedException;

    Evaluation updateEvaluation(Long id, EvaluationUpdateRequest evaluationUpdateRequest, Long accountId)
            throws EvaluationIdNotExistedException, AccountIdNotExistedException;

    Evaluation createEvaluation(EvaluationCreateRequest evaluationCreateRequest, Long accountId);
}
