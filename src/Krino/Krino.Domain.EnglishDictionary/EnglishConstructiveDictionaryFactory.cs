using Krino.Domain.ConstructiveAdpositionalGrammar.ConstructiveDictionaries;
using Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticConstructions;
using Krino.Domain.EnglishGrammar.LinguisticConstructions;

namespace Krino.Domain.EnglishDictionary
{
    public class EnglishConstructiveDictionaryFactory
    {
        public int MaxWords { get; set; } = 7;

        public Pattern RootPattern { get; set; } = EnglishPattern.e_Period_I;

        public IConstructiveDictionary Create()
        {
            var rootPatterns = new Pattern[] { RootPattern };
            var patternConstructions = new PatternConstructions(MaxWords, PatternProvider.Patterns, rootPatterns);
            var result = new ConstructiveDictionary(MorphemeProvider.AttributesModel, patternConstructions, MorphemeProvider.Morphemes);
            return result;
        }
    }
}
