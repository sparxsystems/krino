using Krino.ConstructiveGrammar.LinguisticStructures;
using Krino.ConstructiveGrammar.LinguisticStructures.Attributes;
using Krino.ConstructiveGrammar.LinguisticStructures.Rules;
using Krino.Vertical.Utils.Rules;

namespace Krino.EnglishGrammar.Morphology
{
    public static class EnglishWordRules
    {
        public static RuleBase<IWord> IsLexical() => WordRules.WordContainsAttribute(GrammarAttributes.Morpheme.Free.Lexical);

        public static RuleBase<IWord> IsNoun() => WordRules.WordContainsAttribute(GrammarAttributes.Morpheme.Free.Lexical.Noun);
        public static RuleBase<IWord> IsNounInBaseForm() => WordRules.WordContainsAttribute(GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular);


        public static RuleBase<IWord> IsAdjective() => WordRules.WordContainsAttribute(GrammarAttributes.Morpheme.Free.Lexical.Adjective);

        public static RuleBase<IWord> IsAdverb() => WordRules.WordContainsAttribute(GrammarAttributes.Morpheme.Free.Lexical.Adverb);


        public static RuleBase<IWord> IsVerb() => WordRules.WordContainsAttribute(GrammarAttributes.Morpheme.Free.Lexical.Verb);

        public static RuleBase<IWord> IsVerbInBaseForm() =>
            WordRules.WordContainsAttribute(GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base.Singular) &
            !WordRules.WordContainsAttribute(GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base.Singular.ThirdPerson);

        public static RuleBase<IWord> IsIrregularVerb() => WordRules.WordContainsAttribute(GrammarAttributes.Morpheme.Free.Lexical.Verb.Irregular);


        public static WordBeginsWithStrRule WordBeginsWithStr(string beginningOfStr) => new WordBeginsWithStrRule(beginningOfStr);

        public static BeginsWithPhonemesRule BeginsWithPhonemes(params Phoneme[] phonemes) => new BeginsWithPhonemesRule(phonemes);
        public static WordBeginsWithPhonemesRule WordBeginsWithPhonemes(params Phoneme[] phonemes) => new WordBeginsWithPhonemesRule(phonemes);

        public static EndsWithPhonemesRule EndsWithPhonemes(params Phoneme[] phonemes) => new EndsWithPhonemesRule(phonemes);
    }
}
