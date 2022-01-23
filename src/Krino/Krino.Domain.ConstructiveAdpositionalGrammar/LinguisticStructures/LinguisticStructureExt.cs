using System;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticStructures
{
    public static class LinguisticStructureExt
    {
        public static void AddSubStructure(this ILinguisticStructure parent, ILinguisticStructure subStructure)
        {
            if (parent is IPhrase term)
            {
                term.Words.Add((IWord)subStructure);
            }
            else if (parent is IPredicate predicate)
            {
                predicate.Phrases.Add((IPhrase)subStructure);
            }
            else if (parent is IClause clause)
            {
                if (subStructure is ISubject subject)
                {
                    clause.Subject = subject;
                }
                else
                {
                    clause.Predicate = (IPredicate)subStructure;
                }
            }
            else if (parent is ISentence sentence)
            {
                sentence.Clauses.Add((IClause)subStructure);
            }
            else
            {
                throw new InvalidOperationException($"Invalid parent type '{parent.GetType().Name}'");
            }
        }
    }
}
