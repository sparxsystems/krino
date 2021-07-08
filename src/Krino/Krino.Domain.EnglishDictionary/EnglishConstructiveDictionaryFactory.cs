using Krino.Domain.ConstructiveAdpositionalGrammar.ConstructiveDictionaries;
using Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticConstructions;

namespace Krino.Domain.EnglishDictionary
{
    public class EnglishConstructiveDictionaryFactory
    {
        public int MaxWords { get; set; } = 7;

        public IConstructiveDictionary Create()
        {
            var patternConstructions = new PatternConstructions(MaxWords, PatternProvider.Patterns);
            var result = new ConstructiveDictionary(MorphemeProvider.AttributesModel, patternConstructions, MorphemeProvider.Morphemes);
            return result;
        }
    }
}
