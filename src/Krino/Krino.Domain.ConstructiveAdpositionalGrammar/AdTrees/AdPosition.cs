namespace Krino.Domain.ConstructiveAdpositionalGrammar.AdTrees
{
    public class AdPosition : IAdTreeItem
    {
        public AdPosition(GrammarCharacter grammarCharacter)
        {
            GrammarCharacter = grammarCharacter;
        }

        public GrammarCharacter GrammarCharacter { get; private set; }
    }
}
