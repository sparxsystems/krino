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
                
                case LinguisticStructureType.Predicate:
                    {
                        result = StructureAttributes.Predicate;
                        break;
                    }
                case LinguisticStructureType.Verb:
                    {
                        result = StructureAttributes.Verb;
                        break;
                    }


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

            // TODO: !!!
            //if (StructureAttributes.NounElement.IsIn(attributes))
            //{
            //    result = new Ph(attributes);
            //}

            if (StructureAttributes.Clause.IsIn(attributes))
            {
                result = new Clause(attributes);
            }
            else if (StructureAttributes.AdverbElement.IsIn(attributes))
            {
                result = new Adverb(attributes);
            }
            else if (StructureAttributes.AdjectiveElement.IsIn(attributes))
            {
                result = new AdjectiveElement(attributes);
            }
            else if (StructureAttributes.Subject.IsIn(attributes))
            {
                result = new Subject(attributes);
            }
            else if (StructureAttributes.Predicate.IsIn(attributes))
            {
                result = new Predicate(attributes);
            }
            else if (StructureAttributes.Verb.IsIn(attributes))
            {
                result = new Verb(attributes);
            }

            else if (StructureAttributes.Object.Direct.IsIn(attributes))
            {
                result = new DirectObject(attributes);
            }
            else if (StructureAttributes.Object.Indirect.IsIn(attributes))
            {
                result = new IndirectObject(attributes);
            }
            else if (StructureAttributes.Object.ObjectOfPreposition.IsIn(attributes))
            {
                result = new Obj(attributes);
            }

            else if (StructureAttributes.Complement.SubjectComplement.IsIn(attributes))
            {
                result = new SubjectComplement(attributes);
            }
            else if (StructureAttributes.Complement.ObjectComplement.IsIn(attributes))
            {
                result = new ObjectComplement(attributes);
            }
            else if (StructureAttributes.Complement.AdjectiveComplement.IsIn(attributes))
            {
                result = new AdjectiveComplement(attributes);
            }
            else if (StructureAttributes.Complement.AdverbialComplement.IsIn(attributes))
            {
                result = new AdverbialComplement(attributes);
            }

            if (result == null)
            {
                throw new InvalidOperationException($"Linguistic structure '{type}' is unknown.");
            }

            return result;
        }
    }
}
