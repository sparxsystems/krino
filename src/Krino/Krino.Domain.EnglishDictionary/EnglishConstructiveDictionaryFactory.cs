using Krino.Domain.ConstructiveAdpositionalGrammar.ConstructiveDictionaries;
using Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticConstructions;
using Krino.Domain.EnglishGrammar.LinguisticConstructions;

namespace Krino.Domain.EnglishDictionary
{
    public class EnglishConstructiveDictionaryFactory
    {
        public IConstructiveDictionary Create()
        {
            var rootPatterns = new Pattern[] { EnglishPattern.e_Period_I };
            var patternConstructions = new PatternConstructions(6, PatternProvider.Patterns, rootPatterns);
            var result = new ConstructiveDictionary(MorphemeProvider.AttributesModel, patternConstructions, MorphemeProvider.Morphemes);
            return result;
        }
    }
}
