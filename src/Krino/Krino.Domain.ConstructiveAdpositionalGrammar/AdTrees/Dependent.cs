﻿namespace Krino.Domain.ConstructiveAdpositionalGrammar.AdTrees
{
    public class Dependent : IAdTreeItem
    {
        public Dependent(GrammarCharacter grammarCharacter, Prominence prominence)
        {
            GrammarCharacter = grammarCharacter;
            Prominence = prominence;
        }

        public GrammarCharacter GrammarCharacter { get; private set; }

        public Prominence Prominence { get; private set; }
    }
}
