using Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticStructures;
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
                        result = StructureAttributes.NounElement;
                        break;
                    }
                case LinguisticStructureType.AdjectiveElement:
                    {
                        result = StructureAttributes.AdjectiveElement;
                        break;
                    }
                case LinguisticStructureType.AttributiveAdjective:
                    {
                        result = StructureAttributes.AdjectiveElement.Attributive;
                        break;
                    }
                case LinguisticStructureType.PredicativeAdjective:
                    {
                        result = StructureAttributes.AdjectiveElement.Predicative;
                        break;
                    }
                case LinguisticStructureType.AdverbElement:
                    {
                        result = StructureAttributes.AdverbElement;
                        break;
                    }

                // Objects
                case LinguisticStructureType.DirectObject:
                    {
                        result = StructureAttributes.Object.Direct;
                        break;
                    }
                case LinguisticStructureType.IndirectObject:
                    {
                        result = StructureAttributes.Object.Indirect;
                        break;
                    }
                case LinguisticStructureType.ObjectOfPreposition:
                    {
                        result = StructureAttributes.Object.ObjectOfPreposition;
                        break;
                    }

                // Complements
                case LinguisticStructureType.SubjectComplement:
                    {
                        result = StructureAttributes.Complement.SubjectComplement;
                        break;
                    }
                case LinguisticStructureType.ObjectComplement:
                    {
                        result = StructureAttributes.Complement.ObjectComplement;
                        break;
                    }
                case LinguisticStructureType.AdjectiveComplement:
                    {
                        result = StructureAttributes.Complement.AdjectiveComplement;
                        break;
                    }
                case LinguisticStructureType.AdverbialComplement:
                    {
                        result = StructureAttributes.Complement.AdverbialComplement;
                        break;
                    }


                // Phrases
                case LinguisticStructureType.PrepositionalPhrase:
                    {
                        result = StructureAttributes.PrepositionalPhrase;
                        break;
                    }


                case LinguisticStructureType.Subject:
                    {
                        result = StructureAttributes.Subject;
                        break;
                    }
                case LinguisticStructureType.Verb:
                    {
                        result = StructureAttributes.Verb;
                        break;
                    }

                case LinguisticStructureType.Predicate:
                    {
                        result = StructureAttributes.Predicate;
                        break;
                    }
                

                // Clauses
                case LinguisticStructureType.DeclarativeClause:
                    {
                        result = StructureAttributes.Clause.Declarative;
                        break;
                    }
                case LinguisticStructureType.InterrogativeClause:
                    {
                        result = StructureAttributes.Clause.Interrogative;
                        break;
                    }
                case LinguisticStructureType.ImperativeClause:
                    {
                        result = StructureAttributes.Clause.Imperative;
                        break;
                    }
                case LinguisticStructureType.ExclamativeClause:
                    {
                        result = StructureAttributes.Clause.Exclamative;
                        break;
                    }
                case LinguisticStructureType.NounClause:
                    {
                        result = StructureAttributes.Clause.NounClause;
                        break;
                    }
                case LinguisticStructureType.AdjectiveClause:
                    {
                        result = StructureAttributes.Clause.AdjectiveClause;
                        break;
                    }
                case LinguisticStructureType.AdverbialClause:
                    {
                        result = StructureAttributes.Clause.AdverbialClause;
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
            if (StructureAttributes.NounElement.IsIn(attributes))
            {
                result = new NounElement(attributes);
            }
            else if (StructureAttributes.AdjectiveElement.IsIn(attributes))
            {
                result = new AdjectiveElement(attributes);
            }
            else if (StructureAttributes.AdverbElement.IsIn(attributes))
            {
                result = new AdverbElement(attributes);
            }

            // Objects
            else if (StructureAttributes.Object.Direct.IsIn(attributes) ||
                     StructureAttributes.Object.Indirect.IsIn(attributes) ||
                     StructureAttributes.Object.ObjectOfPreposition.IsIn(attributes))
            {
                result = new Objectt(attributes);
            }

            // Complements
            else if (StructureAttributes.Complement.SubjectComplement.IsIn(attributes) ||
                     StructureAttributes.Complement.ObjectComplement.IsIn(attributes) ||
                     StructureAttributes.Complement.AdjectiveComplement.IsIn(attributes) ||
                     StructureAttributes.Complement.AdverbialComplement.IsIn(attributes))
            {
                result = new Complement(attributes);
            }

            // Phrases
            else if (StructureAttributes.PrepositionalPhrase.IsIn(attributes))
            {
                result = new PrepositionalPhrase(attributes);
            }


            else if (StructureAttributes.Subject.IsIn(attributes))
            {
                result = new Subject(attributes);
            }
            else if (StructureAttributes.Verb.IsIn(attributes))
            {
                result = new Verb(attributes);
            }


            else if (StructureAttributes.Predicate.IsIn(attributes))
            {
                result = new Predicate(attributes);
            }
            

            else if (StructureAttributes.Clause.IsIn(attributes))
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
