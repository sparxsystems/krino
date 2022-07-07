using Krino.Domain.ConstructiveGrammar.Parsing;

namespace Krino.App.Services
{
    public interface IGrammarService
    {
        Task<Parser> CreateParser();
    }
}
