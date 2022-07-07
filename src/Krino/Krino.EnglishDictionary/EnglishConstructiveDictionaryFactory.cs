﻿using Krino.ConstructiveGrammar.Dictionary;

namespace Krino.EnglishDictionary
{
    public class EnglishConstructiveDictionaryFactory
    {
        public IConstructiveDictionary2 Create()
        {
            var result = new ConstructiveDictionary2(MorphemeProvider.Morphemes);
            return result;
        }
    }
}
