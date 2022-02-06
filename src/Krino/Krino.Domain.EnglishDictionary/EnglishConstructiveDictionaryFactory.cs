using Krino.Domain.ConstructiveAdpositionalGrammar.ConstructiveDictionaries;

namespace Krino.Domain.EnglishDictionary
{
    public class EnglishConstructiveDictionaryFactory
    {
        public int MaxWords { get; set; } = 7;

        public IConstructiveDictionary2 Create()
        {
            var result = new ConstructiveDictionary2(MorphemeProvider.Morphemes);
            return result;
        }
    }
}
