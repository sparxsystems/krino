using Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticStructures.Attributes;
using System;
using System.Numerics;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticStructures
{
    public class LinguisticStructureFactory
    {
        public ILinguisticStructure Create(BigInteger attributes)
        {
            ILinguisticStructure result = null;

            // Specialized phrases.
            if (GrammarAttributes.VerbElement.IsIn(attributes))
            {
                result = new VerbElement(attributes);
            }

            // General phrases
            else if (GrammarAttributes.NounElement.IsIn(attributes) ||
                     GrammarAttributes.AdjectiveElement.IsIn(attributes) ||
                     GrammarAttributes.AdverbElement.IsIn(attributes) ||
                     GrammarAttributes.AdverbialAdjunct.IsIn(attributes) ||

                     GrammarAttributes.Object.IsIn(attributes) ||
                     GrammarAttributes.Complement.IsIn(attributes) ||
                     GrammarAttributes.Phrase.IsIn(attributes))
            {
                result = new Phrase(attributes);
            }


            else if (GrammarAttributes.Subject.IsIn(attributes))
            {
                result = new Subject(attributes);
            }
            else if (GrammarAttributes.Predicate.IsIn(attributes))
            {
                result = new Predicate(attributes);
            }


            else if (GrammarAttributes.Clause.IsIn(attributes))
            {
                result = new Clause(attributes);
            }

            else if (GrammarAttributes.Sentence.IsIn(attributes))
            {
                result = new Sentence(attributes);
            }



            if (result == null)
            {
                var type = GrammarAttributes.Instance.GetFullName(attributes);
                throw new InvalidOperationException($"Linguistic structure '{type}' is unknown.");
            }

            return result;
        }
    }
}
