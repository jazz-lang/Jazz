#include <string>
#include <vector>

#include <llvm/ADT/ArrayRef.h>
#include <llvm/ADT/StringRef.h>

namespace jazz {

class PackageManifest {
public:
    class Dependency {
    public:
        Dependency(std::string packageIdentifier, std::string packageVersion)
        : packageIdentifier(std::move(packageIdentifier)), packageVersion(std::move(packageVersion)) {}
        llvm::StringRef getPackageIdentifier() const { return packageIdentifier; }
        llvm::StringRef getPackageVersion() const { return packageVersion; }
        std::string getGitRepositoryUrl() const;
        std::string getFileSystemPath() const;

    private:
        std::string packageIdentifier;
        std::string packageVersion;
    };

public:
    PackageManifest(std::string&& packageRoot);
    llvm::StringRef getPackageName() const { return packageName; }
    llvm::ArrayRef<Dependency> getDeclaredDependencies() const { return declaredDependencies; }
    llvm::ArrayRef<std::string> getLinkLibraries() const {return linkLibraries;}
    std::vector<std::string> getTargetRootDirectories() const;
    bool isMultiTarget() const { return multitarget; }
    llvm::StringRef getOutputDirectory() const { return outputDirectory; }
    static const char manifestFileName[];

public: 
    std::vector<std::string> linkLibraries;
private:
    std::string packageRoot;
    std::string packageName;
    std::vector<Dependency> declaredDependencies;
    
    bool multitarget;
    std::string outputDirectory;
};

} // namespace jazz
