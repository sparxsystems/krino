using Krino.Vertical.Utils.Enums;

namespace Krino.ConstructiveGrammar.LinguisticStructures.Attributes.Sememes
{
    /// <summary>
    /// Noun semantic attributes.
    /// </summary>
    public class NounSememes : EnumGroupBase
    {
        public NounSememes(EnumGroupBase parent) : base(parent)
        {
            Gender = new GenderSememes(this);
            Number = new NumberSememes(this);
            Case = new CaseSememes(this);
        }

        public GenderSememes Gender { get; }

        public NumberSememes Number { get; }

        public CaseSememes Case { get; }
    }
}
