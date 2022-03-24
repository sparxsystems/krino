using Krino.Vertical.Utils.Enums;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticStructures.Attributes
{
    public class VerbNumberForm : EnumGroupBase
    {
        public VerbNumberForm(EnumGroupBase parent) : base(parent)
        {
            Singular = new VerbPersonForm(this);
            Plural = new VerbPersonForm(this);
        }

        public VerbPersonForm Singular { get; }
        public VerbPersonForm Plural { get; }
    }
}
