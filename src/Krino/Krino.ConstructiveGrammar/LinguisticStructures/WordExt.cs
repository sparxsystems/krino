using System.Linq;

namespace Krino.ConstructiveGrammar.LinguisticStructures
{
    public static class WordExt
    {
        public static bool HasSuffix(this IWord word, string suffix) 
            => word.Suffixes.Where(x => x != null).LastOrDefault()?.Value == suffix;

        public static bool HasPrefix(this IWord word, string prefix)
            => word.Prefixes.Where(x => x != null).FirstOrDefault()?.Value == prefix;
    }
}
