using Krino.ConstructiveGrammar.LinguisticStructures;
using Krino.ConstructiveGrammar.LinguisticStructures.Attributes;
using Krino.ConstructiveGrammar.LinguisticStructures.Rules;
using Krino.Vertical.Utils.Rules;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Krino.EnglishGrammar.Morphology
{
    public static class EnglishWordRules
    {
        public static RuleBase<IWord> IsNounInBaseForm() =>
            WordRules.WordContainsAttribute(GrammarAttributes.Morpheme.Free.Lexical.Noun.Sememe.Number.Singular);

        public static RuleBase<IWord> IsVerbInBaseForm() =>
            WordRules.WordContainsAttribute(GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base.Singular) &
            !WordRules.WordContainsAttribute(GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base.Singular.ThirdPerson);

        public static RuleBase<IWord> IsIrregularVerb() => WordRules.WordContainsAttribute(GrammarAttributes.Morpheme.Free.Lexical.Verb.Irregular);
    }
}
