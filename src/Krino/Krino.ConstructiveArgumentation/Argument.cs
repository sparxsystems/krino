using Krino.ConstructiveGrammar.LinguisticStructures;
using System.Collections.Generic;
using System.Linq;

namespace Krino.ConstructiveArgumentation
{
    public class Argument
    {
        public Argument(ISentence sentence)
        {
            Sentence = sentence;
        }

        public ISentence Sentence { get; }


        public IClause Premise => Sentence.IndependentClauses[0].DependentClauses.First();

        public IClause Conclusion => Sentence.IndependentClauses[0].IndependentClause;

        public ArgumentForm Form
        {
            get
            {
                ArgumentForm result;

                var premiseSubject = Premise.Subject.Value;
                var premisePredicate = Premise.Predicate.Value;

                var conclusionSubject = Conclusion.Subject.Value;
                var conclustionPredicate = Conclusion.Predicate.Value;

                if (premiseSubject == conclusionSubject && premisePredicate != conclustionPredicate)
                {
                    result = ArgumentForm.a_is_X_because_a_is_Y;
                }
                else if (premiseSubject != conclusionSubject && premisePredicate == conclustionPredicate)
                {
                    result = ArgumentForm.a_is_X_because_b_is_X;
                }
                else
                {
                    result = ArgumentForm.q_is_T_because_r_is_T;
                }

                return result;
            }
        }


        public IReadOnlyCollection<ISentence> Lever { get; private set; }
    }
}
