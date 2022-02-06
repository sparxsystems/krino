using Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticStructures.Attributes;
using Krino.Vertical.Utils.Enums;
using System;
using System.Numerics;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticStructures
{
    public class LinguisticStructureFactory
    {
        public ILinguisticStructure Create(BigInteger attributes)
        {
            ILinguisticStructure result = null;

            // Elements
            if (GrammarAttributes.NounElement.IsIn(attributes))
            {
                result = new NounElement(attributes);
            }
            else if (GrammarAttributes.AdjectiveElement.IsIn(attributes))
            {
                result = new AdjectiveElement(attributes);
            }
            else if (GrammarAttributes.AdverbElement.IsIn(attributes))
            {
                result = new AdverbElement(attributes);
            }
            else if (GrammarAttributes.VerbElement.IsIn(attributes))
            {
                result = new VerbElement(attributes);
            }

            // Objects
            else if (GrammarAttributes.Object.ObjectOfVerb.Direct.IsIn(attributes) ||
                     GrammarAttributes.Object.ObjectOfVerb.Indirect.IsIn(attributes) ||
                     GrammarAttributes.Object.ObjectOfPreposition.IsIn(attributes))
            {
                result = new Objectt(attributes);
            }

            // Complements
            else if (GrammarAttributes.Complement.SubjectComplement.IsIn(attributes) ||
                     GrammarAttributes.Complement.ObjectComplement.IsIn(attributes) ||
                     GrammarAttributes.Complement.AdjectiveComplement.IsIn(attributes) ||
                     GrammarAttributes.Complement.AdverbialComplement.IsIn(attributes))
            {
                result = new Complement(attributes);
            }

            // Phrases
            else if (GrammarAttributes.PrepositionalPhrase.IsIn(attributes))
            {
                result = new PrepositionalPhrase(attributes);
            }
            else if (GrammarAttributes.InfinitivePhrase.IsIn(attributes))
            {
                result = new InfinitivePhrase(attributes);
            }


            else if (GrammarAttributes.Subject.IsIn(attributes))
            {
                result = new Subject(attributes);
            }
            else if (GrammarAttributes.Verb.IsIn(attributes))
            {
                result = new Verb(attributes);
            }


            else if (GrammarAttributes.Predicate.IsIn(attributes))
            {
                result = new Predicate(attributes);
            }


            else if (GrammarAttributes.Clause.IsIn(attributes))
            {
                result = new Clause(attributes);
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
