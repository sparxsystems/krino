using Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticStructures;
using Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticStructures.Attributes;
using System;
using System.Numerics;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Parsing
{
    internal static class LinguisticStructureTypeExt
    {
        public static BigInteger GetAttributes(this LinguisticStructureType type)
        {
            BigInteger result;

            switch (type)
            {
                // Elements
                case LinguisticStructureType.NounElement:
                    {
                        result = GrammarAttributes.NounElement;
                        break;
                    }
                case LinguisticStructureType.AdjectiveElement:
                    {
                        result = GrammarAttributes.AdjectiveElement;
                        break;
                    }
                case LinguisticStructureType.AttributiveAdjective:
                    {
                        result = GrammarAttributes.AdjectiveElement.Attributive;
                        break;
                    }
                case LinguisticStructureType.PostpositiveAdjective:
                    {
                        result = GrammarAttributes.AdjectiveElement.PostPositive;
                        break;
                    }
                case LinguisticStructureType.PredicativeAdjective:
                    {
                        result = GrammarAttributes.AdjectiveElement.Predicative;
                        break;
                    }
                case LinguisticStructureType.AdverbElement:
                    {
                        result = GrammarAttributes.AdverbElement;
                        break;
                    }

                // Objects
                case LinguisticStructureType.DirectObject:
                    {
                        result = GrammarAttributes.Object.ObjectOfVerb.Direct;
                        break;
                    }
                case LinguisticStructureType.IndirectObject:
                    {
                        result = GrammarAttributes.Object.ObjectOfVerb.Indirect;
                        break;
                    }
                case LinguisticStructureType.ObjectOfPreposition:
                    {
                        result = GrammarAttributes.Object.ObjectOfPreposition;
                        break;
                    }

                // Complements
                case LinguisticStructureType.SubjectComplement:
                    {
                        result = GrammarAttributes.Complement.SubjectComplement;
                        break;
                    }
                case LinguisticStructureType.ObjectComplement:
                    {
                        result = GrammarAttributes.Complement.ObjectComplement;
                        break;
                    }
                case LinguisticStructureType.AdjectiveComplement:
                    {
                        result = GrammarAttributes.Complement.AdjectiveComplement;
                        break;
                    }
                case LinguisticStructureType.AdverbialComplement:
                    {
                        result = GrammarAttributes.Complement.AdverbialComplement;
                        break;
                    }


                // Phrases
                case LinguisticStructureType.PrepositionalPhrase:
                    {
                        result = GrammarAttributes.PrepositionalPhrase;
                        break;
                    }
                case LinguisticStructureType.InfinitivePhrase:
                    {
                        result = GrammarAttributes.InfinitivePhrase;
                        break;
                    }


                case LinguisticStructureType.Subject:
                    {
                        result = GrammarAttributes.Subject;
                        break;
                    }
                case LinguisticStructureType.Verb:
                    {
                        result = GrammarAttributes.Verb;
                        break;
                    }

                case LinguisticStructureType.Predicate:
                    {
                        result = GrammarAttributes.Predicate;
                        break;
                    }
                

                // Clauses
                case LinguisticStructureType.DeclarativeClause:
                    {
                        result = GrammarAttributes.Clause.Declarative;
                        break;
                    }
                case LinguisticStructureType.InterrogativeClause:
                    {
                        result = GrammarAttributes.Clause.Interrogative;
                        break;
                    }
                case LinguisticStructureType.ImperativeClause:
                    {
                        result = GrammarAttributes.Clause.Imperative;
                        break;
                    }
                case LinguisticStructureType.ExclamativeClause:
                    {
                        result = GrammarAttributes.Clause.Exclamative;
                        break;
                    }
                case LinguisticStructureType.NounClause:
                    {
                        result = GrammarAttributes.Clause.NounClause;
                        break;
                    }
                case LinguisticStructureType.AdjectiveClause:
                    {
                        result = GrammarAttributes.Clause.AdjectiveClause;
                        break;
                    }
                case LinguisticStructureType.AdverbialClause:
                    {
                        result = GrammarAttributes.Clause.AdverbialClause;
                        break;
                    }

                default:
                    {
                        result = 0;
                        break;
                    }
            }

            return result;
        }

        public static ILinguisticStructure GetLinguisticStructure(this LinguisticStructureType type)
        {
            ILinguisticStructure result = null;

            var attributes = type.GetAttributes();

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
                throw new InvalidOperationException($"Linguistic structure '{type}' is unknown.");
            }

            return result;
        }
    }
}
