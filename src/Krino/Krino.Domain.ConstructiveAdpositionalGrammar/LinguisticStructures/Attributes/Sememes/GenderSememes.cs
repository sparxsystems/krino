using Krino.Vertical.Utils.Enums;

namespace Krino.Domain.ConstructiveGrammar.LinguisticStructures.Attributes.Sememes
{
    /// <summary>
    /// Gender attributes.
    /// </summary>
    public class GenderSememes : EnumGroupBase
    {
        public GenderSememes(EnumGroupBase parent) : base(parent)
        {
            Masculine = new EnumValue(this);
            Feminine = new EnumValue(this);
            Neuter = new EnumValue(this);
        }

        public EnumValue Masculine { get; }

        public EnumValue Feminine { get; }

        public EnumValue Neuter { get; }
    }
}
