{ localLib ? import ./../../../lib.nix
, stateDir ? localLib.maybeEnv "CARDANO_STATE_DIR" "./state-demo"
, config ? {}
, runWallet ? true
, runExplorer ? false
, stats ? true
, numCoreNodes ? 4
, system ? builtins.currentSystem
, pkgs ? import localLib.fetchNixPkgs { inherit system config; }
, gitrev ? localLib.commitIdFromGitRepo ./../../../.git
, ghcRuntimeArgs ? "-N2 -qg -A1m -I0 -T"
, additionalNodeArgs ? ""
}:

with localLib;

let
  executables =  {
    corenode = "${iohkPkgs.cardano-sl-node-static}/bin/cardano-node-simple";
    wallet = "${iohkPkgs.cardano-sl-wallet-new}/bin/cardano-node";
    launcher = "${iohkPkgs.cardano-sl-tools}/bin/cardano-launcher";
    explorer = "${iohkPkgs.cardano-sl-explorer-static}/bin/cardano-explorer";
  };
  ifWallet = localLib.optionalString (runWallet);
  iohkPkgs = import ./../../../default.nix { inherit config system pkgs gitrev; };
  src = ./../../../.;
  configFiles = pkgs.runCommand "cardano-config" {} ''
    mkdir -pv $out
    cd $out
    cp -vi ${iohkPkgs.cardano-sl.src + "/configuration.yaml"} configuration.yaml
    cp -vi ${iohkPkgs.cardano-sl.src + "/mainnet-genesis-dryrun-with-stakeholders.json"} mainnet-genesis-dryrun-with-stakeholders.json
    cp -vi ${iohkPkgs.cardano-sl.src + "/mainnet-genesis.json"} mainnet-genesis.json
  '';
in pkgs.writeScript "demo-cluster" ''
  #!${pkgs.stdenv.shell}
  source ${src + "/scripts/common-functions.sh"}
  LOG_TEMPLATE=${src + "/log-configs/template-demo.yaml"}
  function make_yaml_list {
    local items="$1"
    echo "$items" | sed 's/+RTS.*-RTS//g' | tr " " "\n" | grep -vE '^$' | while read l; do
      echo "- \"$l\""
    done
  }
  function stop_cardano {
    trap "" INT TERM
    echo "Received TERM!"
    echo "Stopping Cardano core nodes"
    for pid in ''${core_pid[@]}
    do
      echo killing pid $pid
      kill $pid
    done
    ${ifWallet ''
      echo killing wallet pid $wallet_pid
    kill $wallet_pid
    ''}
    wait
    echo "Stopped all Cardano processes, exiting!"
    exit 0
  }
  system_start=$((`date +%s` + 15))
  echo "Using system start time "$system_start

  echo "Generating Topology"
  mkdir -p ${stateDir}
  gen_kademlia_topology ${builtins.toString (numCoreNodes + 1)} ${stateDir}

  trap "stop_cardano" INT TERM
  echo "Launching a demo cluster..."
  for i in {0..${builtins.toString (numCoreNodes - 1)}}
  do
    node_args="$(node_cmd $i "" "$system_start" "${stateDir}" "" "${stateDir}/logs" "${stateDir}") --configuration-file ${configFiles}/configuration.yaml"
    echo Launching core node $i with args: $node_args
    ${executables.corenode} $node_args &> /dev/null &
    core_pid[$i]=$!

  done
  ${ifWallet ''
    if [ ! -d ${stateDir}/tls-files ]; then
      mkdir -p ${stateDir}/tls-files
      ${pkgs.openssl}/bin/openssl req -x509 -newkey rsa:2048 -keyout ${stateDir}/tls-files/server.key -out ${stateDir}/tls-files/server.crt -days 30 -nodes -subj "/CN=localhost"
    fi
    echo Launching wallet node:
    i=${builtins.toString numCoreNodes}
    wallet_args=" --tlscert ${stateDir}/tls-files/server.crt --tlskey ${stateDir}/tls-files/server.key --tlsca ${stateDir}/tls-files/server.crt"
    wallet_args="$wallet_args --new-wallet --wallet-address 127.0.0.1:8090 --wallet-debug"
    node_args="$(node_cmd $i "$wallet_args" "$system_start" "${stateDir}" "" "${stateDir}/logs" "${stateDir}") --configuration-file ${configFiles}/configuration.yaml"
    echo Running wallet with args: $node_args
    ${executables.wallet} $node_args &
    wallet_pid=$!
  ''}
  sleep infinity
''
