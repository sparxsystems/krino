using System;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticStructures
{
    public static class LinguisticStructureExt
    {
        public static void AddSubStructure(this ILinguisticStructure parent, ILinguisticStructure subStructure)
        {
            if (parent is IPhrase term)
            {
                term.Items.Add((IPhraseItem)subStructure);
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
                if (subStructure is IClause internalClause)
                {
                    sentence.Clauses.Add(internalClause);
                }
                else if (subStructure is IWord punctuation)
                {
                    sentence.PunctuationMark = punctuation.Root;
                }
            }
            else if (parent is IText text)
            {
                text.Sentences.Add((ISentence)subStructure);
            }
            else
            {
                throw new InvalidOperationException($"Invalid parent type '{parent.GetType().Name}'");
            }
        }
    }
}
