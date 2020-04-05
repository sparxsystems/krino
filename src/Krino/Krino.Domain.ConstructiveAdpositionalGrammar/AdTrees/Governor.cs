namespace Krino.Domain.ConstructiveAdpositionalGrammar.AdTrees
{
    public class Governor : IAdTreeItem
    {
        public Governor(GrammarCharacter grammarCharacter, Prominence prominence)
        {
            GrammarCharacter = grammarCharacter;
            Prominence = prominence;
        }

        public GrammarCharacter GrammarCharacter { get; private set; }

        public Prominence Prominence { get; private set; }
    }
}
